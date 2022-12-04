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
            required: true

        },

        date: {
            type: String,
            required: true
        },

        pathIDList: {
            type: [String],
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
