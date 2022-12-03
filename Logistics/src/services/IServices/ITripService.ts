import { Result } from "../../core/logic/Result";
import { ITripDTO } from "../../dto/ITripDTO";


export default interface ITripService {

   exist(TripID: string): Promise<Result<boolean>>;
   createTrip(Trip: ITripDTO): Promise<Result<ITripDTO>>;
   getTrip(TripID: string): Promise<Result<ITripDTO>>;
   getAllTrips(): Promise<Result<ITripDTO[]>>;
   updateTrip(Trip: ITripDTO): Promise<Result<ITripDTO>>;
   deleteTrip(TripID: string): Promise<Result<ITripDTO>>;
}