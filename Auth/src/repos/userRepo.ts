import { Service, Inject } from 'typedi';

import { Document, FilterQuery, Model } from 'mongoose';
import { IUserPersistence } from '../dataschema/IUserPersistence';

import IUserRepo from "../services/IRepos/IUserRepo";
import { User } from "../domain/user/User";
import { UserId } from "../domain/user/UserId";
import { UserEmail } from "../domain/user/UserEmail";
import { UserMap } from "../mappers/UserMap";

@Service()
export default class UserRepo implements IUserRepo {

  constructor(
    @Inject('userSchema') private userSchema : Model<IUserPersistence & Document>,
  ) { }

  public async exists (user: User): Promise<boolean> {

    const idX = user.id instanceof UserId ? (<UserId>user.id): user.id;

    const query = { domainId: idX}; 
    const userDocument = await this.userSchema.findById( query );

    return !!userDocument === true;
  }

  public async save (user: User): Promise<User> {
    const query = { userId: user.userId.id }; 
    const userDocument = await this.userSchema.findOne( query as FilterQuery<IUserPersistence & Document> );
    try {
      if (userDocument === null ) {
        const rawUser: any = UserMap.toPersistence(user);

        const userCreated = await this.userSchema.create(rawUser);

        return UserMap.toDomain(userCreated);
      } else {
        userDocument.userId = user.userId.id;
        userDocument.firstName = user.firstName.firstName;
        userDocument.lastName = user.lastName.lastName;
        userDocument.email = user.email.email;
        userDocument.password= user.password.password;
        userDocument.role= user.role.id;
        await userDocument.save();

        return user;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findByEmail (email: UserEmail | string): Promise<User> {
    const query = { email: email.toString() };
    const userRecord = await this.userSchema.findOne( query );

    if( userRecord != null) {
      return UserMap.toDomain(userRecord);
    }
    else
      return null;
  }

  public async findById (userId:UserId): Promise<User> {
    const query = {userId: userId}
    
    const userDocument = await this.userSchema.findOne(query as FilterQuery<IUserPersistence & Document>);
    
    if(userDocument!=null){
      return UserMap.toDomain(userDocument);
    }
    else {
      return null;
    }
    
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
    const query={UserId: User.userId.id};
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