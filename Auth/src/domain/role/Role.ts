import { AggregateRoot } from "../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";

import { Result } from "../../core/logic/Result";
import { RoleId } from "./RoleId";
import { RoleName } from "./RoleName";

import IRoleDTO from "../../dto/IRoleDTO";

interface RoleProps {
  name: RoleName;
  roleId: RoleId;
}

export class Role extends AggregateRoot<RoleProps> {
  get id (): UniqueEntityID {
    return this._id;
  }

  get roleId (): RoleId {
    return this.props.roleId
  }

  get name (): RoleName {
    return this.props.name;
  }

  set name ( name: RoleName) {
    this.props.name = name;
  }
  private constructor (props: RoleProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (roleDTO: IRoleDTO, id?: UniqueEntityID): Result<Role> {
    try {
      const role = new Role({
        name: RoleName.create(roleDTO.name).getValue(),
        roleId: RoleId.create(roleDTO.roleId).getValue()
      },id)
      return Result.ok<Role>(role);
    } catch (error) {
      return Result.fail<Role>(error);
    }
    

  }
}
