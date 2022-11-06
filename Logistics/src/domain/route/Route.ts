import { AggregateRoot } from "../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { Result } from "../../core/logic/Result";
import { IRouteDTO } from "../../dto/IRouteDTO";
import { RouteID } from "./RouteID";
import { Date } from "./Date"
import { TruckPlate } from "./TruckPlate";
import { Warehouses } from "./warehouses";


interface RouteProps {
    routeID: RouteID;
    date: Date;
    truckPlate: TruckPlate;
    warehouse: Warehouses;
}

export class Route extends AggregateRoot<RouteProps> {
    get id (): UniqueEntityID{
        return this._id;
    }

    get routeID (): RouteID {
        return this.props.routeID;
    }

    get date (): Date {
        return this.props.date;
    }

    get truckPlate(): TruckPlate{
        return this.props.truckPlate;
    }

    get warehouse(): Warehouses{
        return this.props.warehouse;
    }

    private constructor(props: RouteProps, id?:UniqueEntityID){
        super(props,id);
    }

    public static create ( routeDTO: IRouteDTO, id?:UniqueEntityID): Result<Route>{
        try{
            const route = new Route({
                routeID: RouteID.create(routeDTO.id).getValue(),
                date: Date.create(routeDTO.date).getValue(),
                truckPlate:TruckPlate.create(routeDTO.truckPlate).getValue(),
                warehouse: Warehouses.create(routeDTO.warehouses).getValue()
            }, id);
            return Result.ok<Route>(route);
        }catch(error) {
            return Result.fail<Route>(error);
        
        } 
    }
}
