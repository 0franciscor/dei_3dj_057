import config from "../../config";
import { Inject, Service } from "typedi";
import IPathRepo from "../repos/IRepos/IPathRepo";
import IPathService from "./IServices/IPathService";
import { Path } from "../domain/path/Path"
import { Result } from "../core/logic/Result";
import { IPathDTO } from "../dto/IPathDTO";
import { PathMap } from "../mappers/PathMap";
import { StartWHId } from "../domain/path/StartWHId";
import { DestinationWHId } from "../domain/path/DestinationWHId";
import { PathDistance } from "../domain/path/PathDistance";
import { PathTravelTime } from "../domain/path/PathTravelTime";
import { WastedEnergy } from "../domain/path/WastedEnergy";
import { ExtraTravelTime } from "../domain/path/ExtraTravelTime";


@Service()
export default class PathService implements IPathService{
    constructor(
        @Inject(config.repos.path.name) private pathRepo: IPathRepo,
    ){}

    public async createPath(pathDTO: IPathDTO): Promise<Result<IPathDTO>> {
        try{
            const path = await this.pathRepo.getPathById(pathDTO.pathID);
            if(path !== null)
                return Result.fail<IPathDTO>("Path already exists");
            const pathOrError = Path.create(pathDTO);
            
            if(pathOrError.isFailure){
                return Result.fail<IPathDTO>(pathOrError.error);
            }
            const pathResult = pathOrError.getValue();
            
            await this.pathRepo.save(pathResult);
            
            const pathDTOResult = PathMap.toDTO(pathResult) as IPathDTO;
            
            return Result.ok<IPathDTO>(pathDTOResult)
        }catch(e){
            throw e;
        }
    }

    public async getPath(pathID: string): Promise<Result<IPathDTO>> {
        try {   
            const path = await this.pathRepo.getPathById(pathID);
            if(path === null)
                return Result.fail<IPathDTO>("Path not found");
            
            const pathDTOResult = PathMap.toDTO(path) as IPathDTO;
            return Result.ok<IPathDTO>(pathDTOResult);
        } catch (e) {
            throw e;
        }
    }

    public async getAllPath(pathDTO: IPathDTO): Promise<Result<IPathDTO[]>> {
        try{

            const paths = await this.pathRepo.getAllPaths(pathDTO.startWHId, pathDTO.destinationWHId);
            const pathDTOResult = PathMap.toDTOList(paths) as IPathDTO[];
            return Result.ok<IPathDTO[]>(pathDTOResult);
        }catch(e){
            throw e;
        }
    }

    public async updatePath(pathDTO: IPathDTO): Promise<Result<IPathDTO>> {
        try{
            const path = await this.pathRepo.getPathById(pathDTO.pathID);
            if(path===null)
                return Result.fail<IPathDTO>("Path not found");
            if(pathDTO.startWHId != path.startWHId.startWHId && pathDTO.startWHId != null)
                path.startWHId = StartWHId.create(pathDTO.startWHId).getValue();
            if(pathDTO.destinationWHId != path.destinationWHId.destinationWHId && pathDTO.destinationWHId != null)
                path.destinationWHId= DestinationWHId.create(pathDTO.destinationWHId).getValue();
            if(pathDTO.pathDistance != path.pathDistance.pathDistance && pathDTO.pathDistance != null)
                path.pathDistance = PathDistance.create(pathDTO.pathDistance).getValue();
            if(pathDTO.pathTravelTime != path.pathTravelTime.pathTravelTime && pathDTO.pathTravelTime != null)
                path.pathTravelTime = PathTravelTime.create(pathDTO.pathTravelTime).getValue();
            if(pathDTO.wastedEnergy != path.wastedEnergy.wastedEnergy && pathDTO.wastedEnergy != null)
                path.wastedEnergy= WastedEnergy.create(pathDTO.wastedEnergy).getValue();
            if(pathDTO.extraTravelTime != path.extraTravelTime.extraTravelTime && pathDTO.extraTravelTime != null)
                path.extraTravelTime = ExtraTravelTime.create(pathDTO.extraTravelTime).getValue();
            await this.pathRepo.save(path);

            const pathDTOResult = PathMap.toDTO(path) as IPathDTO;
            return Result.ok<IPathDTO>(pathDTOResult);
        }catch(e){
            throw e;
        }
    }

    public async deletePath(pathID: string): Promise<Result<IPathDTO>> {
        try{
            const path = await this.pathRepo.getPathById(pathID);
            console.log(path); 
            if(path === null)
                return Result.fail<IPathDTO>("Path not found");
            await this.pathRepo.delete(path.pathID);

            const pathDTOResult = PathMap.toDTO(path) as IPathDTO;
            return Result.ok<IPathDTO>(pathDTOResult);
        }catch(e){
            throw e;
        }
    }
}