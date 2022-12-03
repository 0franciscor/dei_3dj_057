import { Repo } from "../../core/infra/Repo";
import { Trip } from "../../domain/trip/Trip";
import { TripID } from "../../domain/trip/TripID";


export default interface ITripRepo extends Repo<Trip>{
    exists(trip: Trip): Promise<boolean>;
    save(trip: Trip): Promise<Trip>;
    delete(trip: Trip): Promise<Trip>;
    getTripById(tripID: TripID|string): Promise<Trip>;
    getAllTrips(): Promise<Trip[]>;

}