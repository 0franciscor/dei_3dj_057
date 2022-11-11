import { IRoutePersistence } from "../../dataschema/IRoutePersistence";
import mongoose  from "mongoose";

const RouteSchema = new mongoose.Schema(
    {
        domainId: {
            type: String,
            unique: true,
        },

        routeID: {
            type: String,
            unique: true,
            required: true

        },

        date: {
            type: String,
            required: true
        },

        pathIDlist: {
            type: String,
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


export default mongoose.model<IRoutePersistence & mongoose.Document>('Route',RouteSchema);
