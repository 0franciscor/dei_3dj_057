
import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface RoleIdProps{
    id:string;
}
export class RoleId extends ValueObject<RoleIdProps> {
    get id():string{
        return this.props.id;
    }

    private constructor(props: RoleIdProps){
        super(props);
    }

    public static create(id:string):Result<RoleId>{
        if(id.length<=0){
            return Result.fail<RoleId>('RoleId must be greater than 0');
        }

        return Result.ok<RoleId>(new RoleId({id}));
    }
}