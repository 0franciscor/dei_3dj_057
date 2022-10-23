import { AggregateRoot } from "../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { Result } from "../../core/logic/Result";
import { RouteID } from "./RouteID";
import { StartWHId } from "./StartWHId";
import { DestinationWHId } from "./DestinationWHId";
import { RouteDistance } from "./RouteDistance";
import { RouteTravelTime } from "./RouteTravelTime";
import { WastedEnergy } from "./WastedEnergy";
import { ExtraTravelTime } from "./ExtraTravelTime";

interface RouteProps{
    startWHId: StartWHId;
    destinationWHId: DestinationWHId;
    routeDistance: RouteDistance;
    routeTravelTime: RouteTravelTime;  
    wastedEnergy: WastedEnergy;
    extraTravelTime: ExtraTravelTime;
}


export class Route extends AggregateRoot<RouteProps>{
    get id(): UniqueEntityID{
        return this._id;
    }


    get routeID (): RouteID{
        return new RouteID(this.routeID.toValue());
    }

    get startWHId (): StartWHId{
        return this.props.startWHId;
    }

    get destinationWHId(): DestinationWHId{
        return this.props.destinationWHId;
    }

    get routeDistance(): RouteDistance{
        return this.props.routeDistance;
    }

    get routeTravelTime(): RouteTravelTime{
        return this.props.routeTravelTime;
    }

    get wastedEnergy(): WastedEnergy{
        return this.props.wastedEnergy;
    }

    get extraTravelTime(): ExtraTravelTime{
        return this.props.extraTravelTime;
    }

    private constructor (props: RouteProps, id?: UniqueEntityID){
        super(props,id);
    } 
}