
import { Inject, Service } from "typedi";
import config from "../../config";
import { Result } from "../core/logic/Result";
import { PackagingID } from "../domain/packaging/PackagingID";
import { Date } from "../domain/trip/Date";
import { Trip } from "../domain/trip/Trip";
import { TruckID } from "../domain/truck/TruckID";
import { ITripDTO } from "../dto/ITripDTO";
import { TripMap } from "../mappers/TripMap";
import ITripRepo from "./IRepos/ITripRepo";
import ITripService from "./IServices/ITripService";


@Service()
export default class TripService implements ITripService {
    constructor(
        @Inject(config.repos.trip.name) private tripRepo: ITripRepo,
     ) { }


    public async exist(tripID: string): Promise<Result<boolean>> {
        try {
            const tripResult = await this.tripRepo.getTripById(tripID);
            if(tripResult === null)
                return Result.ok<boolean>(false);
            return Result.ok<boolean>(true);
        }catch(e){
            throw e;
        }
    }

    public async createTrip(tripDTO: ITripDTO): Promise<Result<ITripDTO>> {
        try{
            
            const trip = await this.tripRepo.getTripById(tripDTO.tripID);
            
            if(trip !== null)
                return Result.fail<ITripDTO>("Trip already exists");
            const tripOrError = Trip.create(tripDTO);
            
            if(tripOrError.isFailure) {
                return Result.fail<ITripDTO>(tripOrError.error);
            }
            
            const tripResult = tripOrError.getValue();
            
            const test = await this.tripRepo.save(tripResult);
            
            const tripDTOResult = TripMap.toDTO(tripResult) as ITripDTO;
            return Result.ok<ITripDTO>(tripDTOResult);
        }catch(e){
            throw e;
        }
    } 


    public async getTrip(tripID: string): Promise<Result<ITripDTO>>{
        try{
            
            const trip = await this.tripRepo.getTripById(tripID);
            if(trip === null)
                return Result.fail<ITripDTO>("Trip not found");

            const tripDTOResult = TripMap.toDTO(trip) as ITripDTO;
            return Result.ok<ITripDTO>(tripDTOResult);

        }catch (e){
            throw e;
        }
    }

    public async getAllTrips(): Promise<Result<ITripDTO[]>> {
        try{
            const trips = await this.tripRepo.getAllTrips();
            const tripDTOResult = TripMap.toDTOList(trips) as ITripDTO[];
            return Result.ok<ITripDTO[]>(tripDTOResult);
        }catch(e){
            throw e;
        }       
    }

    public async updateTrip(tripDTO: ITripDTO): Promise<Result<ITripDTO>> {
        try {
            const trip = await this.tripRepo.getTripById(tripDTO.tripID);
            if(trip === null)
                return Result.fail<ITripDTO>("Trip not found");

                //retirei update por package
                
            if(tripDTO.truckID !== trip.truck.id && tripDTO.truckID!=null){
                const truckOrError = TruckID.create(tripDTO.truckID);
                if(truckOrError.isFailure){
                    return Result.fail<ITripDTO>(truckOrError.error);
                }

                trip.truck = truckOrError.getValue();
            }
               
            if(tripDTO.date !== trip.date.date && tripDTO.date!=null){
                const dateOrError = Date.create(tripDTO.date);
                if(dateOrError.isFailure) {
                    return Result.fail<ITripDTO>(dateOrError.error);
                }
                trip.date = dateOrError.getValue();
            }

            await this.tripRepo.save(trip);

            const tripDTOResult = TripMap.toDTO(trip) as ITripDTO;
            return Result.ok<ITripDTO>(tripDTOResult);
        }catch(e){
            throw e;
        }

    }

        public async deleteTrip(tripID: string): Promise<Result<ITripDTO>> {
            try{

                const trip = await this.tripRepo.getTripById(tripID);
                if(trip === null)
                    return Result.fail<ITripDTO>("Trip not found");
                   
                await this.tripRepo.delete(trip);
                
                const tripDTOResult = TripMap.toDTO(trip) as ITripDTO;
                return Result.ok<ITripDTO>(tripDTOResult);
            } catch (e){
                throw e;
            }
        }        
    }   

