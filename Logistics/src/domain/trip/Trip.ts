import { AggregateRoot } from "../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { Result } from "../../core/logic/Result";
import { ITripDTO } from "../../dto/ITripDTO";
import { DeliveryID } from "../packaging/DeliveryID";
import { PackagingID } from "../packaging/PackagingID";
import { PathID } from "../path/PathID";
import { TruckID } from "../truck/TruckID";
import { Date } from "./Date";
import { TripID } from "./TripID";


interface TripProps {
    tripID: TripID;
    date: Date;
    pathIDlist: PathID[];
    truckID: TruckID;
    deliveryIDlist: DeliveryID[];
}

export class Trip extends AggregateRoot<TripProps> {
    get id (): UniqueEntityID{
        return this._id;
    }

    get tripID (): TripID {
        return this.props.tripID;
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

    get deliveryIDlist(): DeliveryID[] {
        return this.props.deliveryIDlist;
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

    set deliveryIDlist (deliveryIDlist: DeliveryID[]){
        this.props.deliveryIDlist = deliveryIDlist;
    } 

    

    private constructor(props: TripProps, id?:UniqueEntityID){
        super(props,id);
    }

    public static create ( tripDTO: ITripDTO, id?:UniqueEntityID): Result<Trip>{
        try{

            let list:PathID[]=[];
            if(tripDTO.pathIDlist)
                tripDTO.pathIDlist.forEach(element => {
                    list.push(PathID.create(element).getValue())
                        
                });

            const trip = new Trip({
                tripID: TripID.create(tripDTO.tripID).getValue(),
                date: Date.create(tripDTO.date).getValue(),
                pathIDlist: list,
                truckID: TruckID.create(tripDTO.truckID).getValue(),
                deliveryIDlist: list,
            }, id);
            return Result.ok<Trip>(trip);
        }catch(error) {
            return Result.fail<Trip>(error);
        
        } 
    }
}
