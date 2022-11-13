import { Repo } from "../../core/infra/Repo";
import { Role } from "../../domain/role/Role";
import { RoleId } from "../../domain/role/RoleId";
import IRoleDTO from "../../dto/IRoleDTO";

export default interface IRoleRepo extends Repo<Role> {
  save(role: Role): Promise<Role>;
  findById (roleId: RoleId | string): Promise<Role>;
  getAllRoles():Promise<Role[]>    
  delete(role:Role): Promise<Role>;
  //findByIds (rolesIds: RoleId[]): Promise<Role[]>;
  //saveCollection (roles: Role[]): Promise<Role[]>;
  //removeByRoleIds (roles: RoleId[]): Promise<any>
}