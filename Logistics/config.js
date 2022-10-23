import dotenv from 'dotenv';

// Set the NODE_ENV to 'development' by default
process.env.NODE_ENV = process.env.NODE_ENV || 'development';

const envFound = dotenv.config();
if (!envFound) {
  // This error should crash whole process

  throw new Error("⚠️  Couldn't find .env file  ⚠️");
}
export default {

  port: parseInt(process.env.PORT, 10) || 3000,

  logs: {
    level: process.env.LOG_LEVEL || 'info',
  },
  
  databaseURL: process.env.MONGODB_URI || "mongodb://localhost:27017/test",

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
    }
  },

  repos: {
    truck: {
      name: "TruckRepo",
      path: "../repos/truckRepo"
    }
  },

  services: {
    truck: {
      name: "TruckService",
      path: "../services/truckService"
    }
  },
};
