import { Service, Inject } from 'typedi';

import IRouteRepo from './IRepos/IRouteRepo';
import { Route } from '../domain/route/Route';
import { IRoutePersistence } from '../dataschema/IRoutePersistence';
import { RouteMap } from '../mappers/RouteMap';

import { Document, FilterQuery, Model } from 'mongoose';

import { RouteID } from '../domain/route/RouteID';
import { raw} from 'body-parser'; 

@Service()
export default class RouteRepo implements IRouteRepo {

    constructor(
        @Inject('routeSchema') private routeSchema : Model<IRoutePersistence & Document>,
    ) { }

    public async exists(route: Route): Promise<boolean> {
        const idX = route.id instanceof RouteID ? (<RouteID>route.id) : route.id;

        const query = { domainID: idX};
        const routeDocument = await this.routeSchema.findById(query as FilterQuery<IRoutePersistence & Document>);

        return !!routeDocument === true;

    }

    public async save(route: Route): Promise<Route> {
        const query = { routeID: route.routeID.id };
        const routeDocument = await this.routeSchema.findOne( query as FilterQuery<IRoutePersistence & Document>);
        try {

            if(routeDocument === null) {
                const rawRoute: any = RouteMap.toPersistence(route);
                const routeCreated = await this.routeSchema.create(rawRoute);
                return RouteMap.toDomain(routeCreated);
            }
            else{
                routeDocument.routeID = route.routeID.id;
                routeDocument.date = route.date.date;
                routeDocument.warehouses = route.warehouse.warehouse;
                await routeDocument.save();
                return route;
            }
        } catch (error){
            throw error;
        }

    }

    public async delete(route: Route): Promise<Route> {
        const query = { routeID: route.routeID.id };
        const routeDocument = await this.routeSchema.findOne( query as FilterQuery<IRoutePersistence & Document>);

        try{
            if(routeDocument === null) {
                return route;
            }
            else{
                await this.routeSchema.deleteOne(query as FilterQuery<IRoutePersistence & Document>);
                return route;
            }
        } catch(error){
            throw error;
        }

    }

    public async getRouteById(id: string): Promise<Route> {

        const query = { routeID: id  };
        const routeDocument = await this.routeSchema.findOne( query as FilterQuery<IRoutePersistence & Document>);
        
        if(routeDocument === null ){
            return null;
        }
        else {
            return RouteMap.toDomain(routeDocument);
        }
    }

    public async getAllRoutes(): Promise<Route[]> {
        const routeDocument = await this.routeSchema.find();
        let routes: Route[] = [];
        routeDocument.forEach(route => {
            routes.push(RouteMap.toDomain(route));
        });
        return routes;
    }


}