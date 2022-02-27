import board
import busio
import select
import sys

print("shittycpu40 Pi Pico console code")

print("Initialising Pi Pico UART")

uart = busio.UART(tx=board.GP0, rx=board.GP1, baudrate = 300, timeout=0.05)
poller = select.poll()

# poller.register(uart, select.POLLIN)
poller.register(sys.stdin, select.POLLIN)
while True:
    # print("P", end="")
    ps = poller.poll(10)
    for tup in ps:
        obj = tup[0]
        if obj == uart:
            data = uart.read(1)
            if data is not None:
              try:
                print(data.decode("utf-8"), end="")
              except Exception as e:
                print(e)
        elif obj == sys.stdin:
            data = sys.stdin.read(1)
            uart.write(data.encode("utf-8"))
        else:
            print("X", end="")
    if uart.in_waiting > 0:
            data = uart.read(1)
            if data is not None:
              try:
                print(data.decode("utf-8"), end="")
              except Exception as e:
                print(e)
