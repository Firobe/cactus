BUILD_IN_DOCKER ?= 1
APPLICATION = test
# D1 mini
BOARD ?= esp8266-esp-12x

RIOTBASE ?= /home/virgile/Projects/riot_d1/RIOT

### DEPENDENCIES ###

## SENSORS

# Good sensor
USEMODULE += bmp180
# Bad sensor
USEMODULE += dht
# Auto-init
USEMODULE += saul_default auto_init_saul

## NETWORKING

USEMODULE += esp_wifi
USEMODULE += netdev_default auto_init_gnrc_netif
USEMODULE += gnrc_icmpv6_error gnrc_ipv6_default

# CoAP
USEMODULE += gcoap

## MISC

# Built-in shell
USEMODULE += shell shell_cmds_default
# Time and sleep
USEMODULE += ztimer ztimer_sec ztimer_msec

CFLAGS = \
		 -DWIFI_SSID=\"UristMcWlan\" \
		 -DWIFI_PASS=\"jahuitojcificods2doi\"

include $(RIOTBASE)/Makefile.include
