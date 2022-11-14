
import { Entity } from "../../core/domain/Entity";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface UserIdProps{
  id: string;
}
export class UserId extends ValueObject<UserIdProps> {

  get id (): string {
    return this.id;
  }

  private constructor (props: UserIdProps) {
    super(props)
  }

  public static create (id:string): Result<UserId>{
    if(id.length<=0){
      return Result.fail<UserId>('User id must be greater than 0')
    }
    return Result.ok<UserId>(new UserId({id}))
  }
}