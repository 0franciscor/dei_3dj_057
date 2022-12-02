import { ITripPersistence } from "../../dataschema/ITripPersistence";
import mongoose  from "mongoose";

const TripSchema = new mongoose.Schema(
    {
        domainId: {
            type: String,
            unique: true,
        },

        tripID: {
            type: String,
            unique: true,
            required: true

        },

        date: {
            type: String,
            required: true
        },

        pathIDlist: {
            type: Array,
            required: true
        },

        truckID: {
            type: String,
            required: true
        },

        packagingID: {
            type: String,
            required: true
        },

     
    
    },
    {
        timestamps: true
    }
);


export default mongoose.model<ITripPersistence & mongoose.Document>('Trip',TripSchema);
