import { AggregateRoot } from "../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { Role } from "../role/Role";
import { UserEmail } from "./UserEmail";

import { UserPassword } from "./UserPassword";
import { Result } from "../../core/logic/Result";
import { Guard } from "../../core/logic/Guard";
import { FirstName } from "./FirstName"
import { LastName } from "./LastName"
import { IUserDTO } from "../../dto/IUserDTO";
import { RoleId } from "../role/RoleId";
import { PhoneNumber } from "./PhoneNumber";

interface UserProps{

    firstName: FirstName;
    lastName: LastName;
    email: UserEmail;
    password: UserPassword;
    phoneNumber: PhoneNumber
    role: RoleId
}

export class User extends AggregateRoot<UserProps>{
    get id(): UniqueEntityID{
        return this._id;
    }


    private constructor (props: UserProps, id?: UniqueEntityID){
        super(props,id);
    }

    get email(): UserEmail{
        return this.props.email;
    }

    get firstName(): FirstName{
        return this.props.firstName
    }

    get lastName(): LastName{
        return this.props.lastName
    }
   
    get password (): UserPassword {
        return this.props.password;
    }

    get phoneNumber (): PhoneNumber {
        return this.props.phoneNumber;
    }
    
    get role (): RoleId {
        return this.props.role;
    }
      
    set role (value: RoleId) {
        this.props.role = value;
    }

    set email(value: UserEmail){
        this.props.email = value
    }

    public static create (userDTO: IUserDTO,id?:UniqueEntityID):Result<User>{

        try {
            const user= new User({
                firstName: FirstName.create(userDTO.firstName).getValue(),
                lastName: LastName.create(userDTO.lastName).getValue(),
                email: UserEmail.create(userDTO.email).getValue(),
                password: UserPassword.create(userDTO.password).getValue(),
                phoneNumber: PhoneNumber.create(userDTO.phoneNumber).getValue(),
                role: RoleId.create(userDTO.role).getValue()

            },id)
            return Result.ok<User>(user);
        } catch (error) {
            return Result.fail<User>(error)
        }
    }
}
