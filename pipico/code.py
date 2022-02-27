import board
import busio

print("shittycpu40 Pi Pico console code")

print("Initialising Pi Pico UART")

uart = busio.UART(tx=board.GP0, rx=board.GP1, baudrate = 300, timeout=0.05)

while True:
    data = uart.read(1)
    if data is not None:
        try:
            print(data.decode("utf-8"), end="")
            # print(data, end="")
        except Exception as e:
            print(e)
