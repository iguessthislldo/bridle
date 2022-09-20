from bridle.cdr.serializer import Serializer


def write_file(filename, issues=False):
    print('\n', filename)
    buf = bytearray([0xaa for i in range(0, 100)])
    ser = Serializer(buf, save_details=True)
    s = "Hello, World!"
    i = 0x12345678
    b = True
    print(s, i, b)
    if issues:
        ser.write_string(s, False, 'utf-8')
    else:
        ser.write_s8(s)
    ser.write_u32(i)
    if issues:
        ser.write_u8(2)
    else:
        ser.write_boolean(b)
    print(buf.hex(' '))

    with open(filename, 'wb') as f:
        f.write(buf)

    ser.reset()
    print(ser.read_s8(), ser.read_u32(), ser.read_boolean())
    print(ser.details)


write_file('simple.cdr')
write_file('simple_with_issues.cdr', issues=True)
