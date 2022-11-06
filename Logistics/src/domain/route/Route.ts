import { AggregateRoot } from "../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { Result } from "../../core/logic/Result";
import { IRouteDTO } from "../../dto/IRouteDTO";
import { RouteID } from "./RouteID";
import { Date } from "./Date"
import { Warehouses } from "./Warehouses";




interface RouteProps {
    routeID: RouteID;
    date: Date;
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

    get warehouse(): Warehouses{
        return this.props.warehouse;
    }

    set date (date: Date){
        this.props.date = date;
    }

    set warehouse (warehouse: Warehouses){
        this.props.warehouse = warehouse;
    }

    private constructor(props: RouteProps, id?:UniqueEntityID){
        super(props,id);
    }

    public static create ( routeDTO: IRouteDTO, id?:UniqueEntityID): Result<Route>{
        try{
            console.log(routeDTO);
            const route = new Route({
                routeID: RouteID.create(routeDTO.routeID).getValue(),
                date: Date.create(routeDTO.date).getValue(),
                warehouse: Warehouses.create(routeDTO.warehouses).getValue()
            }, id);
            console.log(route);
            return Result.ok<Route>(route);
        }catch(error) {
            return Result.fail<Route>(error);
        
        } 
    }
}
