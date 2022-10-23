import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface StartWHIdProps{
    startWHId: string;
}

export class StartWHId extends ValueObject<StartWHIdProps>{
    get startWHId(): string{
        return this.props.startWHId;
    }


    private constructor ( props:StartWHIdProps){
        super(props);
    }

    public static create (startWHId: string): Result<StartWHId>{
        if (startWHId==""|| startWHId == null){
            return Result.fail<StartWHId>('Id must be provided')
        }
        return Result.ok<StartWHId>(new StartWHId({ startWHId }));
    }
}