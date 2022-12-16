import { Mapper } from "../core/infra/Mapper";

import {IUserDTO} from "../dto/IUserDTO";

import { User } from "../domain/user/User";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";


import { Model, Document } from 'mongoose';
import { IUserPersistence } from '../dataschema/IUserPersistence';

export class UserMap extends Mapper<User> {

  public static toDTO( user: User): IUserDTO {
    
    return {
      id: user.id.toString(),
      firstName: user.firstName.firstName,
      lastName: user.lastName.lastName,
      email: user.email.email,
      password: user.password.password,
      phoneNumber: user.phoneNumber.phoneNumber,
      role: user.role.id,
    } as IUserDTO;
  }

  public static toDTOList (users:User[]): IUserDTO[]{
    return users.map((user)=> UserMap.toDTO(user));
  }

  public static  toDomain (user: any| Model<IUserPersistence & Document>): User {
  
    const userOrError = User.create(
      user,
      new UniqueEntityID(user._id),
    );
    userOrError.isFailure ? console.log(userOrError):'';
    return userOrError.isSuccess ? userOrError.getValue():null;
  }

  public static toPersistence (user: User): any {
    const a= {
      domainId: user.id.toString(),
      email: user.email.email,
      password: user.password.password,
      phoneNumber: user.phoneNumber.phoneNumber,
      firstName: user.firstName.firstName,
      lastName: user.lastName.lastName,
      role: user.role.id,
    }

    return a
    
  }
}