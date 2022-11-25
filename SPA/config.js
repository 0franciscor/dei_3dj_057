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
  port: parseInt(process.env.PORT, 10) || 3001,

  /**
   * That long string from mlab
   */
  databaseURL: process.env.MONGODB_URI || "mongodb+srv://CappuJSON:FvxPHub044EvYWmW@eletricgo057.accc6c0.mongodb.net/Users",

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
    user: {
      name: "userController",
      path: "../controllers/userController"
    },
    role: {
      name: "roleController",
      path:"../controllers/roleController"
    },
    path: {
      name: "pathController",
      path:"../controllers/pathController"
    },
    warehouse: {
      name: "warehouseController",
      path:"../controllers/warehouseController"
    },
    delivery: {
      name: "deliveryController",
      path:"../controllers/deliveryController"
    },
    truck: {
      name: "truckController",
      path:"../controllers/truckController"
    }
  },

  repos: {
    user: {
      name: "userRepo",
      path: "../repos/userRepo"
    },
    role: {
      name: "roleRepo",
      path: "../repos/roleRepo"
    }
  },

  services: {
    user: {
      name: "userService",
      path: "../services/userService"
    },
    role: {
      name: "roleService",
      path: "../services/roleService"
    }
  },
};