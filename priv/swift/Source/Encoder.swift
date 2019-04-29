import Foundation

func serialize(bert: BertObject?) -> [UInt8]? {
        if bert != nil {
            do {
                let bytes = try Bert.encode(object: bert!)
                var result = [UInt8](repeating: 0, count: bytes.length)
                bytes.getBytes(&result, length: bytes.length)
                return result
            } catch {
                return nil
            }
        }
        return nil
}
    
func stringToBertBinary(input: String?) -> BertObject {
    if let data = input?.data(using: String.Encoding.utf8) {
        let bert = BertBinary(fromNSData: data as NSData)
        return bert
    }
    return BertNil()
}

func stringToBertAtom(input: StringAtom?) -> BertObject {
    if let str = input?.string {
        let bert = BertAtom(fromString: str)
        return bert
    }
    return BertNil()
}

func listFromObjects<T>(input: [T]?) -> BertObject {
    if input == nil {
        return BertNil()
    } else {
        if input!.count == 0 {
            return BertNil()
        } else {
            var result = [BertObject]()
            for i in input! {
                result.append(serialize(object: i as AnyObject))
            }
            return BertList(fromElements: result)
        }
    }
}

func numberFromInt64(input: Int64?) -> BertObject {
    if let id = input {
        return BertNumber(fromInt64: Int64(id))
    } else {
        return BertNil()
    }
}


func serialize(object: AnyObject) -> BertTuple {
    var items = [BertObject]()
    if let instanse = object as? Approve {
        items.append(BertAtom(fromString: "Approve"))
        items.append(stringToBertBinary(input: instanse.id))
        items.append(stringToBertBinary(input: instanse.user))
        items.append(stringToBertBinary(input: instanse.room))
    }
    if let instanse = object as? ok2 {
        items.append(BertAtom(fromString: "ok2"))
        if let s = instanse.src {
            items.append(BertAtom(fromString: s))
        } else {
            items.append(BertNil())
        }
        items.append(stringToBertAtom(input: instanse.code))
    }
    if let instanse = object as? error2 {
        items.append(BertAtom(fromString: "error2"))
        if let s = instanse.src {
            items.append(BertAtom(fromString: s))
        } else {
            items.append(BertNil())
        }
        items.append(stringToBertAtom(input: instanse.code))
    }
    if let instanse = object as? Auth {
        items.append(BertAtom(fromString: "Auth"))
        items.append(stringToBertBinary(input: instanse.token))
        items.append(stringToBertBinary(input: instanse.dev_key))
        items.append(stringToBertBinary(input: instanse.user_id))
        items.append(stringToBertBinary(input: instanse.phone))
        items.append(stringToBertBinary(input: instanse.client_id))
        items.append(stringToBertAtom(input: instanse.type))
        items.append(stringToBertBinary(input: instanse.sms_code))
        if let atempts = instanse.attempts {
            items.append(BertNumber(fromInt64: Int64(atempts)))
        } else {
            items.append(BertNil())
        }
        if let service = instanse.services {
            var result = [BertAtom]()
            for i in service {
                let atom = BertAtom(fromString: i)
                result.append(atom)
            }
            items.append(BertList(fromElements: result))
        } else {
            items.append(BertNil())
        }
    }
    if let instanse = object as? Confirm {
        items.append(BertAtom(fromString: "Confirm"))
        items.append(stringToBertBinary(input: instanse.id))
        if let rosterId = instanse.roster_id {
            items.append(BertNumber(fromInt64: Int64(rosterId)))
        } else {
            items.append(BertNil())
        }
        items.append(stringToBertBinary(input: instanse.friend_id))
        if let atom = instanse.status as? StringAtom {
            items.append(stringToBertAtom(input: atom))
        } else {
            items.append(BertNil())
        }
    }
    if let instanse = object as? Contact {
        items.append(BertAtom(fromString: "Contact"))
        items.append(stringToBertBinary(input: instanse.phone_id))
        items.append(stringToBertBinary(input: instanse.avatar))
        items.append(stringToBertBinary(input: instanse.names))
        items.append(stringToBertBinary(input: instanse.surnames))
        items.append(stringToBertBinary(input: instanse.person_id))
        if let atom = instanse.status as? StringAtom {
            items.append(stringToBertAtom(input: atom))
        } else {
            items.append(BertNil())
        }

    }
    if let instanse = object as? error {
        items.append(BertAtom(fromString: "error"))
        if let string = instanse.code as? String {
            items.append(stringToBertBinary(input: string))
        } else if let string = instanse.code as? StringAtom {
            items.append(stringToBertAtom(input: string))
        } else {
            items.append(BertNil())
        }
    }
    if let instanse = object as? Friend {
        items.append(BertAtom(fromString: "Friend"))
        items.append(stringToBertBinary(input: instanse.id))
        if let rosterId = instanse.roster_id {
            items.append(BertNumber(fromInt64: Int64(rosterId)))
        } else {
            items.append(BertNil())
        }
        items.append(stringToBertBinary(input: instanse.friend_id))
        if let atom = instanse.status as? StringAtom {
            items.append(stringToBertAtom(input: atom))
        } else {
            items.append(BertNil())
        }

    }
    if let instanse = object as? History {
        items.append(BertAtom(fromString: "History"))
        items.append(stringToBertBinary(input: instanse.roster_id))
        items.append(BertNil())
        if let msgs = instanse.data {
            if msgs.count == 0 {
                items.append(BertNil())
            } else {
                var result = [BertTuple]()
                for i in msgs {
                    result.append(serialize(object: i))
                }
                items.append(BertList(fromElements: result))
            }
        } else {
            items.append(BertNil())
        }
        if let atom = instanse.status as? StringAtom {
            items.append(stringToBertAtom(input: atom))
        } else {
            items.append(BertNil())
        }

    }
    if let instanse = object as? io {
        items.append(BertAtom(fromString: "io"))
        if let o = instanse.code as? ok {
            items.append(serialize(object: o))
        } else if let err = instanse.code as? error {
            items.append(serialize(object: err))
        } else if let o2 = instanse.code as? ok2 {
            items.append(serialize(object: o2))
        } else if let err2 = instanse.code as? error2 {
            items.append(serialize(object: err2))
        } else {
            items.append(BertNil())
        }
        if let o = instanse.data as? String {
            items.append(stringToBertBinary(input: o))
            /* //TODO: Model(value:Tuple(name:"",body:[
            Model(value:Atom()),
            Model(value:Chain(types:[
            Model(value:Binary()),
            Model(value:Number())]))]))]))]))} */
        } else {
            items.append(BertNil())
        }
    }
    if let instanse = object as? Join {
        items.append(BertAtom(fromString: "Join"))
        items.append(stringToBertBinary(input: instanse.id))
        items.append(stringToBertBinary(input: instanse.user))
        items.append(stringToBertBinary(input: instanse.room))
    }
    if let instanse = object as? Leave {
        items.append(BertAtom(fromString: "Leave"))
        items.append(stringToBertBinary(input: instanse.id))
        items.append(stringToBertBinary(input: instanse.user))
        items.append(stringToBertBinary(input: instanse.room))
    }
    if let instanse = object as? Message {
        //TODO:
        items.append(BertAtom(fromString: "Message"))
        items.append(numberFromInt64(input: instanse.id))
        items.append(stringToBertAtom(input: instanse.container))
        items.append(BertNil())
        items.append(numberFromInt64(input: instanse.prev))
        items.append(numberFromInt64(input: instanse.next))
        items.append(BertNil())
        items.append(stringToBertBinary(input: instanse.msg_id))
        items.append(stringToBertBinary(input: instanse.from))
        items.append(stringToBertBinary(input: instanse.to))
        items.append(BertNil())
        items.append(BertNil())
        items.append(BertNil())
        items.append(BertNil())
        if let atom = instanse.status as? StringAtom {
            items.append(stringToBertAtom(input: atom))
        } else {
            items.append(BertNil())
        }

    }
    if let instanse = object as? ok {
        items.append(BertAtom(fromString: "ok"))
        if let string = instanse.code as? String {
            items.append(stringToBertBinary(input: string))
        } else if let string = instanse.code as? StringAtom {
            items.append(stringToBertAtom(input: string))
        } else {
            items.append(BertNil())
        }
    }
    if let instanse = object as? Person {
        //TODO:
        items.append(BertAtom(fromString: "Person"))
        items.append(stringToBertBinary(input: instanse.id))
        items.append(stringToBertBinary(input: instanse.names))
        items.append(stringToBertBinary(input: instanse.surnames))
        items.append(stringToBertBinary(input: instanse.username))
        items.append(BertNil())
        items.append(BertNil())
        items.append(stringToBertBinary(input: instanse.avatar))
        items.append(BertNil())
        items.append(BertNil())
        items.append(BertNil())
        items.append(stringToBertBinary(input: instanse.ThemeID))
        items.append(stringToBertBinary(input: instanse.voxImplantID))
        items.append(BertNil())
        items.append(numberFromInt64(input: instanse.balance))
        if let isParticipants = instanse.isParticipants {
            if isParticipants.count == 0 {
                items.append(BertNil())
            } else {
                var result = [BertObject]()
                for i in isParticipants {
                    if let j = stringToBertBinary(input: i) as? BertBinary {
                        result.append(j)
                    }
                }
                items.append(BertList(fromElements: result))
            }
        } else {
            items.append(BertNil())
        }
        items.append(stringToBertAtom(input: instanse.status))
    }
    if let instanse = object as? Profile {
        items.append(BertAtom(fromString: "Profile"))
        items.append(stringToBertBinary(input: instanse.phone))
        items.append(stringToBertBinary(input: instanse.data))
        items.append(stringToBertBinary(input: instanse.person_id))
        if let accounts = instanse.accounts as? [Roster] {
            if accounts.count == 0 {
                items.append(BertNil())
            } else {
                var result = [BertTuple]()
                for i in accounts {
                    result.append(serialize(object: i))
                }
                items.append(BertList(fromElements: result))
            }
        } else if let accounts = instanse.accounts as? [Int64] {
            if accounts.count == 0 {
                items.append(BertNil())
            } else {
                var result = [BertNumber]()
                for i in accounts {
                    if let n = numberFromInt64(input: i) as? BertNumber {
                        result.append(n)
                    }
                }
                items.append(BertList(fromElements: result))
            }
        } else {
            items.append(BertNil())
        }
        items.append(stringToBertAtom(input: instanse.status))
    }
    if let instanse = object as? Revoke {
        items.append(BertAtom(fromString: "Revoke"))
        items.append(stringToBertBinary(input: instanse.id))
        items.append(stringToBertBinary(input: instanse.user))
        items.append(stringToBertAtom(input: instanse.status))
    }
    if let instanse = object as? Room {
        items.append(BertAtom(fromString: "Room"))
        items.append(stringToBertBinary(input: instanse.desc))
        items.append(listFromObjects(input: instanse.acl))
        items.append(listFromObjects(input: instanse.settings))
    }
    if let instanse = object as? Roster {
        items.append(BertAtom(fromString: "Roster"))
        items.append(numberFromInt64(input: instanse.id))
        items.append(stringToBertBinary(input: instanse.names))
        items.append(stringToBertBinary(input: instanse.surnames))
        items.append(numberFromInt64(input: instanse.size))
        if let userlist = instanse.userlist {
            if userlist.count == 0 {
                items.append(BertNil())
            } else {
                var result = [BertTuple]()
                for i in userlist {
                    result.append(serialize(object: i))
                }
                items.append(BertList(fromElements: result))
            }
        } else {
            items.append(BertNil())
        }
        if let roomlist = instanse.roomlist {
            if roomlist.count == 0 {
                items.append(BertNil())
            } else {
                var result = [BertTuple]()
                for i in roomlist {
                    result.append(serialize(object: i))
                }
                items.append(BertList(fromElements: result))
            }
        } else {
            items.append(BertNil())
        }
        items.append(BertNil()) //TODO
        
    }
    if let instanse = object as? Typing {
        
    }
    return BertTuple(fromElements: items)
}
