import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface RoleNameProps{
    name: string;
}

export class RoleName extends ValueObject<RoleNameProps>{
    get name():string{
        return this.props.name;
    }

    private constructor(props: RoleNameProps){
        super(props);
    }

    public static create(name:string): Result<RoleName>{
        if(name.length==undefined){
            return Result.fail<RoleName>('Role name must be specified')
        }
        return Result.ok<RoleName>(new RoleName({name}));
    }
}