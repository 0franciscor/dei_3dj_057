import { Result } from "../../core/logic/Result";
import { IPathDTO } from "../../dto/IPathDTO";

export default interface IPathService{
    exist(pathID: string): Promise<Result<boolean>>;
    createPath(path:IPathDTO): Promise<Result<IPathDTO>>;
    getPath(pathID:string): Promise<Result<IPathDTO>>;
    getAllPath(path:IPathDTO):Promise<Result<IPathDTO[]>>;
    updatePath(path:IPathDTO):Promise<Result<IPathDTO>>;
    deletePath(pathID:string):Promise<Result<IPathDTO>>;
}