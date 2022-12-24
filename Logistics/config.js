import dotenv from 'dotenv';

// Set the NODE_ENV to 'development' by default
process.env.NODE_ENV = process.env.NODE_ENV || 'development';

const envFound = dotenv.config();
if (!envFound) {
  // This error should crash whole process

  throw new Error("⚠️  Couldn't find .env file  ⚠️");
}

export default {
  /**
   * Your favorite port
   */
  port: parseInt(process.env.PORT, 10) || 3000,

  /**
   * That long string from mlab
   */
  databaseURL: process.env.MONGODB_URI || "mongodb+srv://CappuJSON:FvxPHub044EvYWmW@eletricgo057.accc6c0.mongodb.net/test",

  /**
   * Your secret sauce
   */
  jwtSecret: process.env.JWT_SECRET || "my sakdfho2390asjod$%jl)!sdjas0i secret",

  /**
   * Used by winston logger
   */
  logs: {
    level: process.env.LOG_LEVEL || 'info',
  },

  /**
   * API configs
   */
  api: {
    prefix: '/api',
  },

  controllers: {
    truck: {
      name: "TruckController",
      path: "../controllers/truckController"
    },
    path:{
      name: "PathController",
      path: "../controllers/pathController"
    },
    packaging:{
      name: "PackagingController",
      path: "../controllers/packagingController"
    },

    trip: {
      name: "TripController",
      path: "../controllers/tripController"
    }

  },

  repos: {
    truck: {
      name: "TruckRepo",
      path: "../repos/truckRepo"
    },
    path: {
      name: "PathRepo",
      path: "../repos/pathRepo"
    },
    packaging: {
      name: "PackagingRepo",
      path: "../repos/packagingRepo"
    },
    trip: {
      name: "TripRepo",
      path: "../repos/tripRepo"
    }
  },

  services: {
    truck: {
      name: "TruckService",
      path: "../services/truckService"
    },
    path: {
      name:"PathService",
      path: "../services/pathService"
    },
    packaging: {
      name:"PackagingService",
      path: "../services/packagingService"
    },
    trip: {
      name: "TripService",
      path: "../services/tripService"
    }
  }
};
