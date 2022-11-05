import { Repo } from '../../core/infra/Repo';
import { Path } from '../../domain/path/Path';
import { PathID } from '../../domain/path/PathID';


export default interface IPathRepo extends Repo<Path>{
    exists(path:Path): Promise<boolean>;
    save(path:Path): Promise<Path>;
    delete(Path:Path): Promise<Path>;
    getPathById(pathID:PathID|string): Promise<Path>;
    getAllPaths(startWHId:string, destinationWHId:string): Promise<Path[]>; 
}