import { Result } from "../../core/logic/Result";
import { IUserDTO } from "../../dto/IUserDTO";

export default interface IUserService  {
  //SignUp(userDTO: IUserDTO): Promise<Result<{userDTO: IUserDTO, token: string}>>;
  //SignIn(email: string, password: string): Promise<Result<{ userDTO: IUserDTO, token: string }>>;

  getUser(userId: string): Promise<Result<IUserDTO>>;
  getAllUsers(): Promise <Result<IUserDTO[]>>;
  updateUser(user:IUserDTO):Promise <Result<IUserDTO>>;
  deleteUser(userId:string): Promise <Result<IUserDTO>>
  createUser(user: IUserDTO):Promise <Result<IUserDTO>>;
}
