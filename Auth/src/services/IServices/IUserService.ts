import { Result } from "../../core/logic/Result";
import { IUserDTO } from "../../dto/IUserDTO";

export default interface IUserService  {
  login(user: IUserDTO): Promise<Result<IUserDTO>>;
  getUser(userId: string): Promise<Result<IUserDTO>>;
  getAllUsers(): Promise <Result<IUserDTO[]>>;
  updateUser(user:IUserDTO):Promise <Result<IUserDTO>>;
  deleteUser(userId:string): Promise <Result<IUserDTO>>
  createUser(user: IUserDTO):Promise <Result<IUserDTO>>;
}
