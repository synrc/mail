import Foundation

enum BertError: Error {
    case NotValidBertObject
    case NotValidErlangTerm
    case UnexpectedErlangType
    case IntegerValueToLarge
    case AtomLengthToLarge
}

enum BertType: UInt8 {
    case Version = 131
    case SmallAtom = 115
    case Atom = 100
    case Binary = 109
    case SmallInteger = 97
    case Integer = 98
    case SmallBig = 110
    //        case LargetBig = 111
    //        case Float = 99
    case NewFloat = 70
    case String = 107
    //        case Port = 102
    //        case Pid = 103
    case SmallTuple = 104
    case LargeTuple = 105
    case List = 108
    //        case Reference = 101
    //        case NewReference = 114
    case Nil = 106
}

class BertObject{
    var type: UInt8 = 0
    
    var description : String {
        get {
            return "BertObject"
        }
    }
}

class BertAtom: BertObject {
    var value = ""
    
    init (fromString string: String) {
        value = string
    }
    
    override var description : String {
        get {
            if value == "" {
                return "[]"
            }
            return value
        }
    }
}

class BertBool: BertObject {
    var value: Bool
    
    init (fromBool b: Bool) {
        value = b
    }
    override var description : String {
        get {
            if value.description == "" {
                return "[]"
            }
            return value.description
        }
    }
}

class BertUndefined: BertAtom {
    init () {
        super.init(fromString: "undefined")
    }
    
    override var description : String {
        get {
            if value.description == "" {
                return "[]"
            }
            return value.description
        }
    }
}

class BertBinary: BertObject {
    var value: NSData
    
    init (fromNSData d: NSData) {
        value = d
    }
    
    override var description : String {
        get {
            let d = String(data: value as Data, encoding: String.Encoding.utf8)
            return d ?? "__Can't GET data__"
        }
    }
}

class BertNumber: BertObject {
    var value: Int64
    
    init (fromUInt8 i: UInt8) {
        value = Int64(i)
    }
    
    init (fromInt32 i: Int32) {
        value = Int64(i)
    }
    
    init (fromInt64 i: Int64) {
        value = i
    }
    
    override var description : String {
        get {
            return value.description
        }
    }
}

class BertFloat: BertObject {
    var value: Double
    
    init (fromDouble d: Double) {
        value = d
    }
    override var description : String {
        get {
            return value.description
        }
    }
}

class BertNil: BertObject {
    
    override var description : String {
        get {
            return "NIL"
        }
    }
}

class BertString: BertObject {
    var value: String
    
    init (fromString s: String) {
        value = s
    }
    
    override var description : String {
        get {
            return value
        }
    }
}

class BertTuple: BertObject {
    var elements: [BertObject]
    
    override var description : String {
        get {
            var res = "{"
            for i in 0..<elements.count {
                res += elements[i].description
                if i != elements.count - 1 {
                    res += ","
                }
            }
            res += "}"
            return res
        }
    }
    
    init (fromElements e: [BertObject]) {
        elements = e
    }
    
}

class BertList: BertObject {
    var elements: [BertObject]
    
    init (fromElements e: [BertObject]) {
        elements = e
    }
    
    override var description : String {
        get {
            var res = "{"
            for i in 0..<elements.count {
                res += elements[i].description
                if i != elements.count - 1 {
                    res += ","
                }
            }
            res += "}"
            return res
        }
    }
    
}

class Bert {
    
    class func encode (object: BertObject) throws -> NSData {
        let length = try getEncodeSize(object: object)
        var offset: Int = 0
        var data = [UInt8](repeating: 0, count: length + 1)
        
        data[offset] = BertType.Version.rawValue
        offset += 1
        try encodeInner(object: object, data: &data, offset: &offset)
        
        return NSData(bytes: data, length: length + 1)
    }
    
    class func getObjectClassName(object: BertObject) -> String {
        let className = NSStringFromClass(object_getClass(object)!)
        let classNameArr = className.components(separatedBy: ".")
        
        return classNameArr.last!
    }
    
    class func getEncodeSize (object: BertObject) throws -> Int {
        switch getObjectClassName(object: object) {
        case "BertBool":
            let bool = (object as! BertBool)
            return 1 + 2 + (bool.value ? 4 : 5)
        case "BertUndefined":
            return 1 + 2 + 9
        case "BertAtom":
            let atom = (object as! BertAtom)
            return 1 + 2 + atom.value.characters.count
        case "BertBinary":
            let binary = (object as! BertBinary)
            return 1 + 4 + binary.value.length
        case "BertNil":
            return 1
        case "BertNumber":
            let number = (object as! BertNumber)
            if number.value >= 0 && number.value <= 255 {
                return 1 + 1
            }
            if number.value >= -2147483648 && number.value <= 2147483647 {
                return 1 + 4
            }
            return 1 + 1 + 8
        case "BertFloat":
            return 1 + 8
        case "BertString":
            // TODO: implement encoding for length > 0xFF
            let string = (object as! BertString)
            return 1 + 2 + string.value.characters.count
        case "BertTuple":
            let tuple = (object as! BertTuple)
            var n = 0
            for element in tuple.elements {
                n += try getEncodeSize(object: element)
            }
            return 1 + (tuple.elements.count <= 255 ? 1 : 4) + n
        case "BertList":
            let list = (object as! BertList)
            var n = 0
            for element in list.elements {
                n += try getEncodeSize(object: element)
            }
            return 1 + 4 + 1 + n
        default:
            throw BertError.UnexpectedErlangType
        }
    }
    
    class func encodeInner(object: BertObject, data: inout [UInt8], offset: inout Int) throws {
        switch getObjectClassName(object: object) {
        case "BertAtom":       encodeAtom(atom: object as! BertAtom, data: &data, offset: &offset)
        case "BertBool":       encodeBool(bool: object as! BertBool, data: &data, offset: &offset)
        case "BertUndefined":  encodeAtom(atom: object as! BertUndefined, data: &data, offset: &offset)
        case "BertBinary":     encodeBinary(binary: object as! BertBinary, data: &data, offset: &offset)
        case "BertNumber":     encodeNumber(number: object as! BertNumber, data: &data, offset: &offset)
        case "BertFloat":      encodeFloat(float: object as! BertFloat, data: &data, offset: &offset)
        case "BertString":     encodeString(string: object as! BertString, data: &data, offset: &offset)
        case "BertTuple":  try encodeTuple(tuple: object as! BertTuple, data: &data, offset: &offset)
        case "BertList":   try encodeList(list: object as! BertList, data: &data, offset: &offset)
        case "BertNil":        encodeNil(data: &data, offset: &offset)
        default:
            throw BertError.UnexpectedErlangType
        }
    }
    
    class func encodeAtom(atom: BertAtom, data: inout [UInt8], offset: inout Int) {
        let length = UInt16(atom.value.characters.count)
        data[offset] = BertType.Atom.rawValue
        offset += 1
        writeUInt16(i: length, data: &data, offset: &offset)
        
        memcpy(&data[offset], (atom.value as NSString).utf8String, Int(length))
        offset += Int(length)
    }
    
    class func encodeBool(bool: BertBool, data: inout [UInt8], offset: inout Int) {
        let atom = BertAtom(fromString: (bool.value ? "true" : "false"))
        encodeAtom(atom: atom, data: &data, offset: &offset)
    }
    
    class func encodeNil(data: inout [UInt8], offset: inout Int) {
        data[offset] = BertType.Nil.rawValue
        offset += 1
        //        writeUInt16(i: 2, data: &data, offset: &offset)
        
        //memcpy(&data[offset], (atom.value as NSString).utf8String, 1)
        //offset += 1
    }
    
    class func encodeBinary(binary: BertBinary, data: inout [UInt8], offset: inout Int) {
        data[offset] = BertType.Binary.rawValue
        offset += 1
        let length = UInt32(binary.value.length)
        
        writeUInt32(i: length, data: &data, offset: &offset)
        
        memcpy(&data[offset], binary.value.bytes, Int(length))
        offset += Int(length)
    }
    
    class func encodeNumber(number: BertNumber, data: inout [UInt8], offset: inout Int) {
        if number.value >= 0 && number.value <= 255 {
            data[offset] = BertType.SmallInteger.rawValue
            offset += 1
            data[offset] = UInt8(number.value)
            offset += 1
        } else if number.value >= -2147483648 && number.value <= 2147483647 {
            data[offset] = BertType.Integer.rawValue
            offset += 1
            writeUInt32(i: UInt32(number.value), data: &data, offset: &offset)
        } else {
            data[offset] = BertType.SmallBig.rawValue
            offset += 1
            var i: UInt64 = UInt64(number.value < 0 ? -number.value : number.value)
            var n = 0
            var pos = offset + 2 //arity, sign
            
            while (i > 0) {
                data[pos] = UInt8(i % 256)
                pos += 1
                i = UInt64(floor(Double(i / 256)))
                n += 1
            }
            data[offset] = UInt8(n)
            offset += 1
            data[offset] = UInt8(Int(number.value < 0 ? 1:0))
            offset += n
        }
    }
    
    class func encodeFloat(float: BertFloat, data: inout [UInt8], offset: inout Int) {
        data[offset] = BertType.NewFloat.rawValue
        offset += 1
        let bytes = withUnsafePointer(to: &float.value) {
            $0.withMemoryRebound(to: UInt8.self, capacity: 1, {
                Array(UnsafeBufferPointer(start: $0, count: MemoryLayout<Double>.size))
            })
        }
        var i = UnsafePointer(bytes).withMemoryRebound(to: UInt64.self, capacity: 1) {
            $0.pointee
            }.bigEndian
        
        memcpy(&data[offset],&i,MemoryLayout<Double>.size)
        
        offset += 8
    }
    
    class func encodeString(string: BertString, data: inout [UInt8], offset: inout Int) {
        data[offset] = BertType.String.rawValue
        offset += 1
        writeUInt16(i: UInt16(string.value.characters.count), data: &data, offset: &offset)
        
        memcpy(&data[offset], (string.value as NSString).utf8String, string.value.characters.count)
        offset += string.value.characters.count
    }
    
    class func encodeTuple(tuple: BertTuple, data: inout [UInt8], offset: inout Int) throws {
        if tuple.elements.count <= 255 {
            data[offset] = BertType.SmallTuple.rawValue
            offset += 1
            data[offset] = UInt8(tuple.elements.count)
            offset += 1
        } else {
            data[offset] = BertType.LargeTuple.rawValue
            offset += 1
            writeUInt32(i: UInt32(tuple.elements.count), data: &data, offset: &offset)
        }
        
        for element in tuple.elements {
            try encodeInner(object: element, data: &data, offset: &offset)
        }
    }
    
    class func encodeList(list: BertList, data: inout [UInt8], offset: inout Int) throws {
        data[offset] = BertType.List.rawValue
        offset += 1
        writeUInt32(i: UInt32(list.elements.count), data: &data, offset: &offset)
        
        for element in list.elements {
            try encodeInner(object: element, data: &data, offset: &offset)
        }
        data[offset] = BertType.Nil.rawValue
        offset += 1
    }
    
    class func writeUInt16(i: UInt16, data: inout [UInt8], offset: inout Int) {
        data[offset] = UInt8((i & 0xFF00) >> 8)
        offset += 1
        data[offset] = UInt8(i & 0xFF)
        offset += 1
    }
    
    class func writeUInt32(i: UInt32, data: inout [UInt8], offset: inout Int) {
        data[offset] = UInt8((i & 0xFF000000) >> 24)
        offset += 1
        data[offset] = UInt8((i & 0xFF0000) >> 16)
        offset += 1
        data[offset] = UInt8((i & 0xFF00) >> 8)
        offset += 1
        data[offset] = UInt8(i & 0xFF)
        offset += 1
    }
    
    class func decode (data: NSData) throws -> BertObject {
        if data.length == 0 {
            return BertUndefined()
        }
        
        var offset = 0
        var buffer = [UInt8](repeating: 0, count: 1)
        data.getBytes(&buffer, range: NSMakeRange(offset, 1))
        offset += 1
        let header = buffer[0]
        
        if header != BertType.Version.rawValue {
            throw BertError.NotValidErlangTerm
        }
        
        var printBuffer = [UInt8](repeating: 0, count: data.length)
        data.getBytes(&printBuffer, length: data.length)
        
        return try decodeInner(data: data, offset: &offset)
    }
    
    class func decodeInner (data: NSData, offset: inout Int) throws -> BertObject {
        var buffer = [UInt8](repeating: 0, count: 1)
        
        data.getBytes(&buffer, range: NSMakeRange(offset, 1))
        let type = buffer[0]
        
        switch type {
        case BertType.Atom.rawValue:         return try decodeAtom(data: data, offset: &offset)
        case BertType.SmallAtom.rawValue:    return try decodeAtom(data: data, offset: &offset)
        case BertType.Binary.rawValue:       return     decodeBinary(data: data, offset: &offset)
        case BertType.SmallInteger.rawValue: return try decodeNumber(data: data, offset: &offset)
        case BertType.Integer.rawValue:      return try decodeNumber(data: data, offset: &offset)
        case BertType.SmallBig.rawValue:     return try decodeNumber(data: data, offset: &offset)
        case BertType.NewFloat.rawValue:     return     decodeDouble(data: data, offset: &offset)
        case BertType.String.rawValue:       return     decodeString(data: data, offset: &offset)
        case BertType.SmallTuple.rawValue:   return try decodeTuple(data: data, offset: &offset)
        case BertType.LargeTuple.rawValue:   return try decodeTuple(data: data, offset: &offset)
        case BertType.List.rawValue:         return try decodeList(data: data, offset: &offset)
        case BertType.Nil.rawValue:          return     decodeNil(data: data, offset: &offset)
        default:
            throw BertError.UnexpectedErlangType
        }
    }
    
    class func decodeAtom(data: NSData, offset: inout Int) throws -> BertObject {
        var buffer = [UInt8](repeating: 0, count: 2)
        
        data.getBytes(&buffer, range: NSMakeRange(offset, 1))
        offset += 1
        let type = buffer[0]
        var n: Int
        
        switch type {
        case BertType.Atom.rawValue:
            data.getBytes(&buffer, range: NSMakeRange(offset, 2))
            offset += 2
            let u16 = UnsafePointer(buffer).withMemoryRebound(to: UInt16.self, capacity: 1) {
                $0.pointee
            }
            n = Int(u16.bigEndian)
        case BertType.SmallAtom.rawValue:
            data.getBytes(&buffer, range: NSMakeRange(offset, 1))
            offset += 1
            n = Int(buffer[0])
        default:
            throw BertError.UnexpectedErlangType
        }
        
        var buffer1 = [UInt8](repeating: 0, count: n)
        data.getBytes(&buffer1, range: NSMakeRange(offset, n))
        offset += n
        
        let value = NSString(bytes: buffer1, length: n, encoding: String.Encoding.utf8.rawValue)! as String
        
        switch value {
        case "true":      return BertBool(fromBool: true)
        case "false":     return BertBool(fromBool: false)
        case "undefined": return BertUndefined()
        default:          return BertAtom(fromString: value)
        }
    }
    
    class func decodeBinary(data: NSData, offset: inout Int) -> BertObject {
        offset += 1
        
        var buffer = [UInt8](repeating: 0, count: 4)
        
        data.getBytes(&buffer, range: NSMakeRange(offset, 4))
        offset += 4
        let u16 = UnsafePointer(buffer).withMemoryRebound(to: UInt32.self, capacity: 1) {
            $0.pointee
        }
        let length = Int(u16.bigEndian)
        var dataBuffer = [UInt8](repeating: 0, count: length)
        data.getBytes(&dataBuffer, range: NSMakeRange(offset, length))
        offset += length
        
        return BertBinary(fromNSData: NSData(bytes: dataBuffer, length: length))
    }
    
    class func decodeNumber(data: NSData, offset: inout Int) throws -> BertObject {
        var buffer = [UInt8](repeating: 0, count: 10)
        
        data.getBytes(&buffer, range: NSMakeRange(offset, 1))
        offset += 1
        let type = buffer[0]
        
        switch type {
        case BertType.SmallInteger.rawValue:
            data.getBytes(&buffer, range: NSMakeRange(offset, 1))
            offset += 1
            let value = buffer[0]
            return BertNumber(fromUInt8: value)
        case BertType.Integer.rawValue:
            data.getBytes(&buffer, range: NSMakeRange(offset, 4))
            offset += 4
            let u16 = UnsafePointer(buffer).withMemoryRebound(to: UInt32.self, capacity: 1) {
                $0.pointee
            }
            let value = Int32(u16.bigEndian)
            return BertNumber(fromInt32: value)
        case BertType.SmallBig.rawValue:
            data.getBytes(&buffer, range: NSMakeRange(offset, 1))
            offset += 1
            let arity = buffer[0]
            if (arity > 7) {
                throw BertError.IntegerValueToLarge
            }
            
            data.getBytes(&buffer, range: NSMakeRange(offset, 1))
            offset += 1
            let sign = buffer[0]
            
            var value: Int64 = 0
            var n: Int64 = 1
            for i in 0...arity-1 {
                data.getBytes(&buffer, range:NSMakeRange(offset, 1))
                offset += 1
                let v = Int64(buffer[0])
                value += v * n
                if i+1 != arity {
                    n *= 256
                }
            }
            
            if sign > 0 {
                value = -value
            }
            return BertNumber(fromInt64: Int64(value))
        default:
            throw BertError.UnexpectedErlangType
        }
    }
    
    class func decodeDouble(data: NSData, offset: inout Int) -> BertObject {
        offset += 1
        var buffer = [UInt8](repeating: 0, count: 8)
        data.getBytes(&buffer, range: NSMakeRange(offset, 8))
        offset += 8
        var i = Int64(UnsafePointer(buffer).withMemoryRebound(to: UInt64.self, capacity: 1) {
            $0.pointee
            }.bigEndian)
        var d: Double = 0
        memcpy(&d, &i, MemoryLayout<Int64>.size)
        return BertFloat(fromDouble: d)
    }
    
    class func decodeString(data: NSData, offset: inout Int) -> BertObject {
        offset += 1
        
        var buffer = [UInt8](repeating: 0, count: 2)
        data.getBytes(&buffer, range: NSMakeRange(offset, 2))
        offset += 2
        let length = Int(UnsafePointer(buffer).withMemoryRebound(to: UInt16.self, capacity: 1) {
            $0.pointee
            }.bigEndian)
        
        var stringBuffer = [UInt8](repeating: 0, count: length)
        
        data.getBytes(&stringBuffer, range: NSMakeRange(offset, length))
        offset += length
        
        return BertString(fromString: String(bytes: stringBuffer, encoding: String.Encoding.utf8)!)
    }
    
    class func decodeTuple(data: NSData, offset: inout Int) throws -> BertObject {
        var buffer = [UInt8](repeating: 0, count: 4)
        var n: Int
        
        data.getBytes(&buffer, range: NSMakeRange(offset, 1))
        offset += 1
        let type = buffer[0]
        
        switch type {
        case BertType.SmallTuple.rawValue:
            data.getBytes(&buffer, range: NSMakeRange(offset, 1))
            offset += 1
            n = Int(buffer[0])
        case BertType.LargeTuple.rawValue:
            data.getBytes(&buffer, range: NSMakeRange(offset, 4))
            offset += 4
            n = Int(UnsafePointer(buffer).withMemoryRebound(to: UInt32.self, capacity: 1) {
                $0.pointee
                }.bigEndian)
        default:
            throw BertError.UnexpectedErlangType
        }
        
        var elements = [BertObject]()
        if n > 0 {
            for _ in 0...n-1 {
                elements.append(try decodeInner(data: data, offset: &offset))
            }
        }
        
        return BertTuple(fromElements: elements)
    }
    
    class func decodeList(data: NSData, offset: inout Int) throws -> BertObject {
        offset+=1
        
        var buffer = [UInt8](repeating: 0, count: 4)
        
        data.getBytes(&buffer, range: NSMakeRange(offset, 4))
        offset += 4
        let n = Int(UnsafePointer(buffer).withMemoryRebound(to: UInt32.self, capacity: 1) {
            $0.pointee
            }.bigEndian)
        var elements = [BertObject]()
        if n > 0 {
            for _ in 0...n-1 {
                elements.append(try decodeInner(data: data, offset: &offset))
            }
        }
        
        if data.length > offset {
            data.getBytes(&buffer, range: NSMakeRange(offset, 1))
            if (buffer[0] == BertType.Nil.rawValue) {
                offset += 1
            }
        }
        
        return BertList(fromElements: elements)
    }
    
    class func decodeNil(data: NSData, offset: inout Int) -> BertObject {
        offset += 1
        return BertNil()
    }
}
