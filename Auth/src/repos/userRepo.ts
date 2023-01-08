import { Service, Inject } from 'typedi';

import { Document, FilterQuery, Model } from 'mongoose';
import { IUserPersistence } from '../dataschema/IUserPersistence';

import IUserRepo from "../services/IRepos/IUserRepo";
import { User } from "../domain/user/User";
import { UserEmail } from "../domain/user/UserEmail";
import { UserMap } from "../mappers/UserMap";

@Service()
export default class UserRepo implements IUserRepo {

  constructor(
    @Inject('userSchema') private userSchema : Model<IUserPersistence & Document>,
  ) { }

  public async findById(id: string): Promise<User> {
    const query = { _id: id };
    const userRecord = await this.userSchema.findOne( query as FilterQuery<IUserPersistence & Document> );
    if( userRecord != null) {
      return UserMap.toDomain(userRecord);
    }
    else
      return null;
  }

  public async exists (user: User): Promise<boolean> {

  
    const query = { email: user.email.email}; 
    const userDocument = await this.userSchema.findById( query as FilterQuery<IUserPersistence & Document> );

    return !!userDocument === true;
  }

  public async save (user: User): Promise<User> {
    const query = { email: user.email.email }; 
    const userDocument = await this.userSchema.findOne( query as FilterQuery<IUserPersistence & Document> );
    
    try {
      if (userDocument === null ) {
        
        const rawUser: any = UserMap.toPersistence(user);
        const userCreated = await this.userSchema.create(rawUser);
        return UserMap.toDomain(userCreated);
      } else {
        userDocument.firstName = user.firstName.firstName;
        userDocument.lastName = user.lastName.lastName;
        userDocument.email = user.email.email;
        userDocument.password= user.password.password;
        userDocument.phoneNumber= user.phoneNumber.phoneNumber;
        userDocument.role= user.role.id;
        await userDocument.save();

        return user;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findByEmail (email: UserEmail | string): Promise<User> {
    const query = { email: email.valueOf() };
    const userRecord = await this.userSchema.findOne( query as FilterQuery<IUserPersistence & Document> );
    if( userRecord != null) {
      return UserMap.toDomain(userRecord);
    }
    else
      return null;
  }



  public async findAllUsers():Promise<User[]>{
    const userDocument= await this.userSchema.find();
    let users: User[]=[];
    userDocument.forEach(user=>{
        users.push(UserMap.toDomain(user));
    });
    return users;
  }

  public async deleteUser(User: User): Promise<User> {
    const query={UserEmail: User.email.email};
    const userDocument= await this.userSchema.findOne(query as FilterQuery<IUserPersistence & Document>);
    try {
      if(userDocument == null){
        return User;
      }else{
        await this.userSchema.deleteOne(query as FilterQuery<IUserPersistence & Document>)
        return User;
      }
    } catch (error) {
      
    }
  }
}