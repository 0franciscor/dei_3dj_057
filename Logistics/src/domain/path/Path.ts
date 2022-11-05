import { AggregateRoot } from "../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { Result } from "../../core/logic/Result";
import { PathID } from "./PathID";
import { StartWHId } from "./StartWHId";
import { DestinationWHId } from "./DestinationWHId";
import { PathDistance } from "./PathDistance";
import { PathTravelTime } from "./PathTravelTime";
import { WastedEnergy } from "./WastedEnergy";
import { ExtraTravelTime } from "./ExtraTravelTime";
import { IPathDTO } from "../../dto/IPathDTO";

interface PathProps{
    pathID: PathID;
    startWHId: StartWHId;
    destinationWHId: DestinationWHId;
    pathDistance: PathDistance;
    pathTravelTime: PathTravelTime;  
    wastedEnergy: WastedEnergy;
    extraTravelTime: ExtraTravelTime;
}


export class Path extends AggregateRoot<PathProps>{
    get id(): UniqueEntityID{
        return this._id;
    }


    get pathID (): PathID{
        return this.props.pathID;
    }

    get startWHId (): StartWHId{
        return this.props.startWHId;
    }

    get destinationWHId(): DestinationWHId{
        return this.props.destinationWHId;
    }

    get pathDistance(): PathDistance{
        return this.props.pathDistance;
    }

    get pathTravelTime(): PathTravelTime{
        return this.props.pathTravelTime;
    }

    get wastedEnergy(): WastedEnergy{
        return this.props.wastedEnergy;
    }

    get extraTravelTime(): ExtraTravelTime{
        return this.props.extraTravelTime;
    }

    set startWHId(startWHId : StartWHId){
        this.props.startWHId = startWHId;
    }

    set destinationWHId(destinationWHId : DestinationWHId){
        this.props.destinationWHId = destinationWHId;
    }

    set pathDistance(pathDistance : PathDistance){
        this.props.pathDistance = pathDistance;
    }

    set pathTravelTime(pathTravelTime : PathTravelTime){
        this.props.pathTravelTime = pathTravelTime;
    }

    set wastedEnergy(wastedEnergy : WastedEnergy){
        this.props.wastedEnergy = wastedEnergy;
    }

    set extraTravelTime(extraTravelTime : ExtraTravelTime){
        this.props.extraTravelTime = extraTravelTime;
    }

    private constructor (props: PathProps, id?: UniqueEntityID){
        super(props,id);
    } 

    public static create (pathDTO: IPathDTO, id?: UniqueEntityID): Result<Path>{
        try {
            const path= new Path({
                pathID: PathID.create(pathDTO.pathID).getValue(),
                startWHId: StartWHId.create(pathDTO.startWHId).getValue(), 
                destinationWHId: DestinationWHId.create(pathDTO.destinationWHId).getValue(),
                pathDistance: PathDistance.create(pathDTO.pathDistance).getValue(),
                pathTravelTime: PathTravelTime.create(pathDTO.pathTravelTime).getValue(),
                wastedEnergy: WastedEnergy.create(pathDTO.wastedEnergy).getValue(),
                extraTravelTime: ExtraTravelTime.create(pathDTO.extraTravelTime).getValue(),
            },id);
            return Result.ok<Path>(path);
        } catch (error) {
            return Result.fail<Path>(error);
        }
       
    }
}