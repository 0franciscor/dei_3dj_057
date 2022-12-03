import { Service, Inject } from 'typedi';

import ITripRepo from './IRepos/ITripRepo';
import { Trip } from '../domain/trip/Trip';
import { ITripPersistence } from '../dataschema/ITripPersistence';
import { TripMap } from '../mappers/TripMap';

import { Document, FilterQuery, Model } from 'mongoose';

import { TripID } from '../domain/trip/TripID';
import { raw} from 'body-parser'; 

@Service()
export default class TripRepo implements ITripRepo {

    constructor(
        @Inject('tripSchema') private tripSchema : Model<ITripPersistence & Document>,
    ) { }

    public async exists(trip: Trip): Promise<boolean> {
        const idX = trip.id instanceof TripID ? (<TripID>trip.id) : trip.id;

        const query = { domainID: idX};
        const tripDocument = await this.tripSchema.findById(query as FilterQuery<ITripPersistence & Document>);

        return !!tripDocument === true;

    }

    public async save(trip: Trip): Promise<Trip> {
        const query = { tripID: trip.tripID.id };
        
        const tripDocument = await this.tripSchema.findOne( query as FilterQuery<ITripPersistence & Document>);
       
        try {
            
            if(tripDocument === null) {
                const rawTrip: any = TripMap.toPersistence(trip);
                const tripCreated = await this.tripSchema.create(rawTrip);
                console.log("tripCreated", tripCreated);
                return TripMap.toDomain(tripCreated);
            }
            else{

                let pathToStringList :string[] = [];
                trip.pathIDlist.forEach(pathID => {
                    pathToStringList.push(pathID.id);
                });

                tripDocument.tripID = trip.tripID.id;
                tripDocument.date = trip.date.date;
                tripDocument.pathIDlist= pathToStringList;
                tripDocument.truckID = trip.truck.id;
                tripDocument.packagingID = trip.packaging.id;
                await tripDocument.save();
                return trip;
            }
        } catch (error){
            throw error;
        }
    }

    public async delete(trip: Trip): Promise<Trip> {
        const query = { tripID: trip.tripID.id };
        const tripDocument = await this.tripSchema.findOne( query as FilterQuery<ITripPersistence & Document>);

        try{
            if(tripDocument === null) {
                return trip;
            }
            else{
                await this.tripSchema.deleteOne(query as FilterQuery<ITripPersistence & Document>);
                return trip;
            }
        } catch(error){
            throw error;
        }

    }

    public async getTripById(id: string): Promise<Trip> {

        const query = { tripID: id  };
        const tripDocument = await this.tripSchema.findOne( query as FilterQuery<ITripPersistence & Document>);
        
        if(tripDocument === null ){
            return null;
        }
        else {
            return TripMap.toDomain(tripDocument);
        }
    }

    public async getAllTrips(): Promise<Trip[]> {
        const tripDocument = await this.tripSchema.find();
        let trips: Trip[] = [];
        tripDocument.forEach(trip => {
            trips.push(TripMap.toDomain(trip));
        });
        return trips;
    }


}