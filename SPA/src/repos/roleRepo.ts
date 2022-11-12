import { Service, Inject } from 'typedi';

import IRoleRepo from "../services/IRepos/IRoleRepo";
import { Role } from "../domain/role/Role";
import { RoleId } from "../domain/role/RoleId";
import { RoleMap } from "../mappers/RoleMap";

import { Document, FilterQuery, Model } from 'mongoose';
import { IRolePersistence } from '../dataschema/IRolePersistence';

@Service()
export default class RoleRepo implements IRoleRepo {
  private models: any;

  constructor(
    @Inject('roleSchema') private roleSchema : Model<IRolePersistence & Document>,
  ) {}

  private createBaseQuery (): any {
    return {
      where: {},
    }
  }

  public async exists(role: Role): Promise<boolean> {
    
    const idX = role.id instanceof RoleId ? (<RoleId>role.id) : role.id;

    const query = { domainId: idX}; 
    const roleDocument = await this.roleSchema.findOne( query as FilterQuery<IRolePersistence & Document>);

    return !!roleDocument === true;
  }

  public async save (role: Role): Promise<Role> {
    const query = { domainId: role.id.toString()}; 

    const roleDocument = await this.roleSchema.findOne( query );

    try {
      if (roleDocument === null ) {
        const rawRole: any = RoleMap.toPersistence(role);

        const roleCreated = await this.roleSchema.create(rawRole);

        return RoleMap.toDomain(roleCreated);
      } else {
        roleDocument.name = role.name.name;
        await roleDocument.save();

        return role;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findByDomainId (roleId: RoleId | string): Promise<Role> {
    const query = { domainId: roleId};
    const roleRecord = await this.roleSchema.findOne( query as FilterQuery<IRolePersistence & Document> );

    if( roleRecord != null) {
      return RoleMap.toDomain(roleRecord);
    }
    else
      return null;
  }

  public async getAllRoles(): Promise<Role[]> {
      const roleDocument = await this.roleSchema.find();
      let roles: Role[]=[];
      roleDocument.forEach(role => {roles.push(RoleMap.toDomain(role));
    });
    return roles;
  }

  public async delete(role: Role): Promise<Role>{
    const query = {roleId : role.roleId.id};
    const roleDocument = await this.roleSchema.findOne(query as FilterQuery<IRolePersistence & Document>);
    
    try {
      if(roleDocument === null){
        return role;
      }else{
        await this.roleSchema.deleteOne(query as FilterQuery<IRolePersistence & Document>);
        return role;
      }
    } catch (error) {
      throw error;
    }

  }
}