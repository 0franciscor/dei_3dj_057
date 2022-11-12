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

            if(pathDTO.startWHId !== path.startWHId.startWHId && pathDTO.startWHId != null)
                return Result.fail<IPathDTO>("Starting Warehouse ID cannot be changed");
            if(pathDTO.destinationWHId != path.destinationWHId.destinationWHId && pathDTO.destinationWHId != null)
                return Result.fail<IPathDTO>("Destination Warehouse ID cannot be changed");

            if(pathDTO.pathDistance != path.pathDistance.pathDistance && pathDTO.pathDistance != null){
                    const pathDistanceOrError =PathDistance.create(pathDTO.pathDistance);
                if(pathDistanceOrError.isFailure) {
                    return Result.fail<IPathDTO>(pathDistanceOrError.error);
                }
                path.pathDistance = pathDistanceOrError.getValue();
            }
            if(pathDTO.pathTravelTime != path.pathTravelTime.pathTravelTime && pathDTO.pathTravelTime != null){
                const pathTravelTimeOrError = PathTravelTime.create(pathDTO.pathTravelTime);
                if(pathTravelTimeOrError.isFailure) {
                    return Result.fail<IPathDTO>(pathTravelTimeOrError.error);
                }
                path.pathTravelTime = pathTravelTimeOrError.getValue();
            }
            if(pathDTO.wastedEnergy != path.wastedEnergy.wastedEnergy && pathDTO.wastedEnergy != null){
                const wastedEnergyOrError= WastedEnergy.create(pathDTO.wastedEnergy);
                if(wastedEnergyOrError.isFailure){
                    return Result.fail<IPathDTO>(wastedEnergyOrError.error);
                }
                path.wastedEnergy = wastedEnergyOrError.getValue();
            }
                
            if(pathDTO.extraTravelTime != path.extraTravelTime.extraTravelTime && pathDTO.extraTravelTime != null){
                const extraTravelTimeOrError = ExtraTravelTime.create(pathDTO.extraTravelTime);
                if(extraTravelTimeOrError.isFailure){
                    return Result.fail<IPathDTO>(extraTravelTimeOrError.error);
                }
                path.extraTravelTime = extraTravelTimeOrError.getValue();
            }
                
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
            if(path === null)
                return Result.fail<IPathDTO>("Path not found");
            await this.pathRepo.delete(path);

            const pathDTOResult = PathMap.toDTO(path) as IPathDTO;
            return Result.ok<IPathDTO>(pathDTOResult);
        }catch(e){
            throw e;
        }
    }
}