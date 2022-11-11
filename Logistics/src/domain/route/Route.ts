import { AggregateRoot } from "../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { Result } from "../../core/logic/Result";
import { IRouteDTO } from "../../dto/IRouteDTO";
import { RouteID } from "./RouteID";
import { Date } from "./Date"
import { PathID } from "../path/PathID";
import { TruckID } from "../truck/TruckID";
import { PackagingID } from "../packaging/PackagingID";
import { StartWHId } from "../path/StartWHId";
import { DestinationWHId } from "../path/DestinationWHId";


interface RouteProps {
    routeID: RouteID;
    date: Date;
    pathIDlist: PathID[];
    truckID: TruckID;
    packagingID: PackagingID;
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

    get pathIDlist (): PathID[] {
        return this.props.pathIDlist;
    }

    get truck (): TruckID {
        return this.props.truckID;
    }

    get packaging(): PackagingID {
        return this.props.packagingID;
    }
    
    set date (date: Date){
        this.props.date = date;
    }

    set pathIDlist (pathIDlist: PathID[]) {
        this.props.pathIDlist = pathIDlist;
    }

    set truck (truck: TruckID) {
        this.props.truckID= truck;
    }

    set packaging (packaging: PackagingID){
        this.props.packagingID = packaging;
    } 

    

    private constructor(props: RouteProps, id?:UniqueEntityID){
        super(props,id);
    }

    public static create ( routeDTO: IRouteDTO, id?:UniqueEntityID): Result<Route>{
        try{

            let list:PathID[];

            routeDTO.pathIDlist.forEach(element => {
                list.push(PathID.create(element).getValue(),)
                      
            });

            const route = new Route({
                routeID: RouteID.create(routeDTO.routeID).getValue(),
                date: Date.create(routeDTO.date).getValue(),
                pathIDlist: list,
                truckID: TruckID.create(routeDTO.truckID).getValue(),
                packagingID: PackagingID.create(routeDTO.packagingID).getValue(),
            }, id);
            return Result.ok<Route>(route);
        }catch(error) {
            return Result.fail<Route>(error);
        
        } 
    }
}
