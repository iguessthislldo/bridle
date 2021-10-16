from bridle.cdr.serializer import Serializer

buf = bytearray([0xaa for i in range(0, 100)])
ser = Serializer(buf, save_details=True)
s = "Hello, World!"
i = 0x12345678
b = True
print(s, i, b)
ser.write_s8(s)
ser.write_u32(i)
ser.write_boolean(b)
print(buf.hex(' '))

with open('simple.cdr', 'wb') as f:
    f.write(buf)

ser.reset()
print(ser.read_s8(), ser.read_u32(), ser.read_boolean())
print(ser.details)
