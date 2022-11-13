import { Inject, Service } from "typedi";
import { Result } from "../core/logic/Result";
import { IUserDTO } from "../dto/IUserDTO";
import IUserRepo from "./IRepos/IUserRepo";
import IUserService from "./IServices/IUserService";
import { User } from "../domain/user/User";
import config from "../../config";
import { UserMap } from "../mappers/UserMap";
import { UserEmail } from "../domain/user/UserEmail";


@Service()
export default class UserService implements IUserService{
    constructor(
        @Inject(config.repos.user.name) private userRepo: IUserRepo 
    ){}

    public async createUser(userDTO: IUserDTO): Promise<Result<IUserDTO>> {
        try {
            const user= await this.userRepo.findById(userDTO.userId);
            if(user != null)
                return Result.fail<IUserDTO>("User already exists");
            const userOrError = User.create(userDTO);
            if(userOrError.isFailure){
                return Result.fail<IUserDTO>(userOrError.error);
            }
            const userResult= userOrError.getValue();
            
            await this.userRepo.save(userResult);

            const userDTOresult = UserMap.toDTO(userResult)as IUserDTO;
            return Result.ok<IUserDTO>(userDTOresult)
        } catch (error) {
            throw error;
        }
    }

    public async getAllUsers(): Promise<Result<IUserDTO[]>> {
        try {
            const users = await this.userRepo.findAllUsers()
            const userDTOresult= UserMap.toDTOList(users) as IUserDTO[];
            return Result.ok<IUserDTO[]>(userDTOresult);
        } catch (error) {
            throw error
        }
    }

    public async updateUser(userDTO: IUserDTO): Promise<Result<IUserDTO>> {
        try {  
            const user = await this.userRepo.findById(userDTO.userId);
            if(user===null){
                return Result.fail<IUserDTO>("User not found");
            }
            if(userDTO.email!== user.email.email && userDTO.email!== null){
                const userEmailOrError = UserEmail.create(userDTO.email);
                if(userEmailOrError.isFailure)
                {return Result.fail<IUserDTO>(userEmailOrError.error)};
                user.email = userEmailOrError.getValue();
                
            }
           await this.userRepo.save(user);

           const userDTOresult = UserMap.toDTO(user)as IUserDTO;
           return Result.ok<IUserDTO>(userDTOresult);
            
        } catch (error) {
            throw error
        }
    }

    public async getUser(userId: string): Promise<Result<IUserDTO>> {
        try {
            const user= await this.userRepo.findById(userId);
            if(user==null){
                return Result.fail<IUserDTO>("User not found");
            }
            const userDTOresult= UserMap.toDTO(user) as IUserDTO;
            return Result.ok<IUserDTO>(userDTOresult);
        } catch (error) {
            throw error
        }
    }

    public async deleteUser(userId: string): Promise<Result<IUserDTO>> {
        try {
            const user = await this.userRepo.findById(userId);
            if(user===null){
                return Result.fail<IUserDTO>("user not found");
            }
            await this.userRepo.deleteUser(user);

            const userDTOresult = UserMap.toDTO(user) as IUserDTO;
            return Result.ok<IUserDTO>(userDTOresult);
        } catch (error) {
            throw error
        }
    }
}