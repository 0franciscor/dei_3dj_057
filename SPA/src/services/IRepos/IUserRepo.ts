import { Repo } from '../../core/infra/Repo';
import { User } from "../../domain/user/User";
import { UserEmail } from "../../domain/user/UserEmail";

export default interface IUserRepo extends Repo<User> {
	save(user: User): Promise<User>;
	findByEmail (email: UserEmail | string): Promise<User>;
	findById (id: string): Promise<User>;
	findAllUsers(): Promise<User[]>
	deleteUser(user:User): Promise<User>;
}
  