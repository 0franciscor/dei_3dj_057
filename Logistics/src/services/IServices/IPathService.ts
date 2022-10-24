import { Result } from "../../core/logic/Result";
import { IPathDTO } from "../../dto/IPathDTO";

export default interface IPathService{
    createPath(path:IPathDTO): Promise<Result<IPathDTO>>;
    getPath(pathID:string): Promise<Result<IPathDTO>>;
    getAllPath():Promise<Result<IPathDTO[]>>;
    updatePath(path:IPathDTO):Promise<Result<IPathDTO>>;
    deletePath(pathID:string):Promise<Result<IPathDTO>>;
}