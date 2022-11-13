import { Service, Inject } from 'typedi';
import config from "../../config";
import IRoleDTO from '../dto/IRoleDTO';
import { Role } from "../domain/role/Role";
import IRoleRepo from '../services/IRepos/IRoleRepo';
import IRoleService from './IServices/IRoleService';
import { Result } from "../core/logic/Result";
import { RoleMap } from "../mappers/RoleMap";
import { RoleName } from '../domain/role/RoleName';

@Service()
export default class RoleService implements IRoleService {
  constructor(
      @Inject(config.repos.role.name) private roleRepo : IRoleRepo
  ) {}

  public async getRole( roleId: string): Promise<Result<IRoleDTO>> {
    try {
      const role = await this.roleRepo.findById(roleId);

      if (role === null) {
        return Result.fail<IRoleDTO>("Role not found");
      }
      else {
        const roleDTOResult = RoleMap.toDTO( role ) as IRoleDTO;
        return Result.ok<IRoleDTO>( roleDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }

  public async getAllRoles(): Promise<Result<IRoleDTO[]>> {
      try {
        const roles = await this.roleRepo.getAllRoles();
        const roleDTOResult = RoleMap.toDTOList(roles) as IRoleDTO[];
        return Result.ok<IRoleDTO[]>(roleDTOResult);
      } catch (error) {
        throw error
      }
  }


  public async createRole(roleDTO: IRoleDTO): Promise<Result<IRoleDTO>> {
    try {
      const roleOrError = await Role.create( roleDTO );
      if (roleOrError.isFailure) {
        return Result.fail<IRoleDTO>(roleOrError.errorValue());
      }

      const roleResult = roleOrError.getValue();

      await this.roleRepo.save(roleResult);

      const roleDTOResult = RoleMap.toDTO( roleResult ) as IRoleDTO;
      return Result.ok<IRoleDTO>( roleDTOResult )
    } catch (e) {
      throw e;
    }
  }

  public async updateRole(roleDTO: IRoleDTO): Promise<Result<IRoleDTO>> {
    try {
      const role = await this.roleRepo.findById(roleDTO.roleId);

      if (role === null) {
        return Result.fail<IRoleDTO>("Role not found");
      }
      else {
        role.name = RoleName.create(roleDTO.name).getValue();
        await this.roleRepo.save(role);

        const roleDTOResult = RoleMap.toDTO( role ) as IRoleDTO;
        return Result.ok<IRoleDTO>( roleDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }

  public async deleteRole(roleId: string): Promise<Result<IRoleDTO>> {
      try {
        const role = await this.roleRepo.findById(roleId);
        if(role===null){
          return Result.fail<IRoleDTO>("Role not found")
        }
        await this.roleRepo.delete(role);
        const roleDTOResult =RoleMap.toDTO(role) as IRoleDTO;
        return Result.ok<IRoleDTO>(roleDTOResult)
      } catch (error) {
        throw error
      }
  }

}
