#include <stdio.h>
#include <stdlib.h>

#include "shell.h"
#include "ztimer.h"
#include "saul_reg.h"
#include "net/gcoap.h"

int16_t get_temp_raw(saul_reg_t* sensor) {
    phydat_t out;
    switch (saul_reg_read(sensor, &out)) {
        case -ENODEV:
        case -ENOTSUP:
        case -ECANCELED:
            return -1;
        case 1:
            assert(out.unit == UNIT_TEMP_C);
            assert(out.scale == -1);
            return out.val[0];
        default:
            return -1;
    }
}

#define MAX_RETRIES 20
int16_t get_temp(saul_reg_t* sensor) {
    int16_t temp = get_temp_raw(sensor);
    int retries = 0;
    while(temp == -1) {
        ++retries;
        if(retries >= MAX_RETRIES) {
            printf("FATAL: reached max retries on reading temp\n");
            exit(1);
        }
        ztimer_sleep(ZTIMER_MSEC, 200);
        temp = get_temp_raw(sensor);
    };
    return temp;
}

#define BUFFER_SIZE 128
/*
 * Send temperature as big-endian on multicast
 */
void send_req(uint16_t data) {
    coap_pkt_t packet;
    uint8_t buffer[BUFFER_SIZE];
    if(gcoap_req_init(&packet, buffer, BUFFER_SIZE, COAP_METHOD_PUT, NULL) != 0) {
        printf("Couldn't init request\n"); return;
    }
    //coap_hdr_set_type(packet.hdr, COAP_TYPE_CON);
    ssize_t hdr_len = coap_opt_finish(&packet, COAP_OPT_FINISH_PAYLOAD);
    if(hdr_len < 0) {
        printf("Couldn't finalize request\n"); return;
    }
    assert(packet.payload_len >= 2);
    *packet.payload = (uint8_t) (data >> 8);
    *(packet.payload + 1) = (uint8_t) (data & 0xFF);
    sock_udp_ep_t remote = { .family = AF_INET6, .port = 5683};
    ipv6_addr_set_all_nodes_multicast(
            (ipv6_addr_t*) &remote.addr.ipv6,
            IPV6_ADDR_MCAST_SCP_LINK_LOCAL
    );
    ssize_t written = gcoap_req_send(buffer, hdr_len + 2, &remote,
            NULL, NULL, NULL, GCOAP_SOCKET_TYPE_UDP);
    switch(written) {
        case 0:
            printf("coudln't send request\n");
            break;
        case -ENOTCONN:
            printf("session establishment failed\n");
            break;
        case -EINVAL:
            printf("tl_type not supported\n");
            break;
        default:
            printf("request sent! %d bytes written\n", written);
            return;
    }
}

void loop(saul_reg_t* sensor) {
    printf("starting loop\n");
    while(1) {
        uint16_t temp = get_temp(sensor);
        printf("sending temp: %d\n", temp);
        send_req(temp);
        ztimer_sleep(ZTIMER_SEC, 10);
    }
}

int main(void) {
    saul_reg_t* temp_sensor = saul_reg_find_type(SAUL_SENSE_TEMP);
    if(!temp_sensor) {
        printf("Couldn't find a temperature sensor!\n");
        goto shell;
    }
    printf("Got a temperature sensor with name '%s'.\n", temp_sensor->name);
    loop(temp_sensor);

shell:
    printf("Now launching shell.\n");
    // Launch shell
    char line_buf[SHELL_DEFAULT_BUFSIZE];
    shell_run(NULL, line_buf, SHELL_DEFAULT_BUFSIZE);
    return EXIT_SUCCESS;
}
