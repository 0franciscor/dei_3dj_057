import { Inject, Service } from "typedi";
import { Result } from "../core/logic/Result";
import { IUserDTO } from "../dto/IUserDTO";
import IUserRepo from "./IRepos/IUserRepo";
import IUserService from "./IServices/IUserService";
import { User } from "../domain/user/User";
import config from "../../config";
import { UserMap } from "../mappers/UserMap";
import { UserEmail } from "../domain/user/UserEmail";
import bcrypt from "bcrypt";
import { resolve } from "path";


@Service()
export default class UserService implements IUserService{
    constructor(
        @Inject(config.repos.user.name) private userRepo: IUserRepo 
    ){}


    


    public async login(user: IUserDTO): Promise<Result<IUserDTO>> {
        try {
            const userOrError = await this.userRepo.findByEmail(user.email);
            if(userOrError == null)
                return Result.fail<IUserDTO>("User not found");
            
            const passwordMatch = await bcrypt.compare(user.password, userOrError.password.password);
            
            if(!passwordMatch)
                return Result.fail<IUserDTO>("Invalid credentials");

            const userDTO = UserMap.toDTO(userOrError) as IUserDTO;
            return Result.ok<IUserDTO>(userDTO);
        } catch (error) {
            throw error;
        }
    }

    public async createUser(userDTO: IUserDTO): Promise<Result<IUserDTO>> {
        try {
            const user= await this.userRepo.findByEmail(userDTO.email);
            if(user != null)
                return Result.fail<IUserDTO>("User already exists");

            //encrypt password
            const salt = await bcrypt.genSalt(10);
            const hashedPassword = await bcrypt.hash(userDTO.password, salt);
            userDTO.password= hashedPassword;
            
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
            const user = await this.userRepo.findByEmail(userDTO.email);
            if(user===null){
                return Result.fail<IUserDTO>("User not found");
            }else{
                user.email= UserEmail.create(userDTO.email).getValue();
                await this.userRepo.save(user);

                const userDTOresult = UserMap.toDTO(user) as IUserDTO;
                return Result.ok<IUserDTO>(userDTOresult)
            }
            
        } catch (error) {
            throw error
        }
    }

    public async getUser(email: string): Promise<Result<IUserDTO>> {
        try {
            const user= await this.userRepo.findByEmail(email);
            if(user==null){
                return Result.fail<IUserDTO>("User not found");
            }
            const userDTOresult= UserMap.toDTO(user) as IUserDTO;
            return Result.ok<IUserDTO>(userDTOresult);
        } catch (error) {
            throw error
        }
    }

    public async getUserByEmail(email: string): Promise<Result<IUserDTO>> {
        try {
            const userOrError = await this.userRepo.findByEmail(email);
            if(userOrError == null)
                return Result.fail<IUserDTO>("User not found");

            const userDTO = UserMap.toDTO(userOrError) as IUserDTO;
            return Result.ok<IUserDTO>(userDTO);
        } catch (error) {
            throw error;
        }
    }

    public async deleteUser(email: string): Promise<Result<IUserDTO>> {
        try {
            const user = await this.userRepo.findByEmail(email);
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