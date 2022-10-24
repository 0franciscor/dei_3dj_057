import { Path } from "../domain/path/Path";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { IPathPersistance } from "../dataschema/IPathPersistance";
import { IPathDTO } from "../dto/IPathDTO";

import { Mapper } from "../core/infra/Mapper"
import { Document , Model  } from "mongoose";

export class PathMap extends Mapper<Path>{
    public static toDTO(path: Path): IPathDTO{
        return{
            id: path.id.toString(),
            pathID: path.pathID.id,
            startWHId: path.startWHId.startWHId,
            destinationWHId: path.destinationWHId.destinationWHId,
            pathDistance: path.pathDistance.pathDistance,
            pathTravelTime: path.pathTravelTime.pathTravelTime,
            wastedEnergy: path.wastedEnergy.wastedEnergy,
            extraTravelTime: path.extraTravelTime.extraTravelTime

        }as IPathDTO
    }

    public static toDTOList(paths: Path[]): IPathDTO[]{
        return paths.map((path) => PathMap.toDTO(path));
    }
    
    public static toDomain(path: any | Model<IPathPersistance & Document>): Path{
        const pathOrError = Path.create(
            path, 
            new UniqueEntityID(path._id),
        );
        pathOrError.isFailure ? console.log(pathOrError.error):'';
        
        return pathOrError.isSuccess ? pathOrError.getValue(): null;
    }

    public static toPersistance (path: Path): any{
        return{
            domainId: path.id.toString(),
            pathID: path.pathID.id,
            startWHId: path.startWHId.startWHId,
            destinationWHId: path.destinationWHId.destinationWHId,
            pathDistance: path.pathDistance.pathDistance,
            pathTravelTime: path.pathTravelTime.pathTravelTime,
            wastedEnergy: path.wastedEnergy.wastedEnergy,
            extraTravelTime: path.extraTravelTime.extraTravelTime
        };
    }
}