import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";
interface PathIDProps{
    id: string;
}

export class PathID extends ValueObject<PathIDProps>{
    get id(): string
    {
        return this.props.id;
    }

    private constructor (props: PathIDProps){
        super(props);
    }

    public static create (id:string): Result<PathID>{
        if(id.length<0){
            return Result.fail<PathID>('PathID must be greater than 0');
        }
        return Result.ok<PathID>(new PathID({id}));
    }
}