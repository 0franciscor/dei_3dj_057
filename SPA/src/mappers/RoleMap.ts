import { Mapper } from "../core/infra/Mapper";

import { Document, Model } from 'mongoose';
import { IRolePersistence } from '../dataschema/IRolePersistence';

import IRoleDTO from "../dto/IRoleDTO";
import { Role } from "../domain/role/Role";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { RoleId } from "../domain/role/RoleId";

export class RoleMap extends Mapper<Role> {
  
  public static toDTO( role: Role): IRoleDTO {
    return {
      id: role.id.toString(),
      roleId: role.roleId.id,
      name: role.name.name
    } as IRoleDTO;
  }

  public static toDomain (role: any | Model<IRolePersistence & Document> ): Role {
    const roleOrError = Role.create(
      role,
      new UniqueEntityID(role.domainId)
    );

    roleOrError.isFailure ? console.log(roleOrError.error) : '';

    return roleOrError.isSuccess ? roleOrError.getValue() : null;
  }

  public static toPersistence (role: Role): any {
    return {
      domainId: role.id.toString(),
      name: role.name.name,
      roleId: role.roleId.id
    }
  }

  public static toDTOList(roles: Role[]): IRoleDTO[] {
    return roles.map((role) => RoleMap.toDTO(role));
}
}