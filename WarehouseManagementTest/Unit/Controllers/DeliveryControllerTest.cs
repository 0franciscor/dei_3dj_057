using EletricGo.Controllers;
using EletricGo.Domain.Deliveries;
using Moq;
using EletricGo.Domain.Shared;
using Microsoft.AspNetCore.Mvc;
using EletricGo.Services;

namespace WarehouseManagementTest.Unit.Controllers
{
    [TestFixture]
    internal class DeliveryControllerTest
    {
        private readonly string? id = "testID";

        private readonly DateTime deliveryDate = new DateTime(2023, 12, 12);

        private readonly float loadTime = 10;

        private readonly float unloadTime = 20;

        private readonly string? destination = "testDestination";

        private readonly float deliveryMass = 30;


        //SETUP LISTS FOR GETALL()
        private List<DeliveryDTO> GetDeliveriesList()
        {
            var deliveryList = new List<DeliveryDTO>
            {
                new DeliveryDTO(){deliveryID = id, deliveryDate = deliveryDate, loadTime = loadTime, unloadTime = unloadTime, destination = destination, deliveryMass = deliveryMass},
                new DeliveryDTO(){deliveryID = "testID2", deliveryDate = deliveryDate, loadTime = loadTime, unloadTime = unloadTime, destination = destination, deliveryMass = deliveryMass},
                new DeliveryDTO(){deliveryID = "testID3", deliveryDate = deliveryDate, loadTime = loadTime, unloadTime = unloadTime, destination = destination, deliveryMass = deliveryMass},
                new DeliveryDTO(){deliveryID = "testID4", deliveryDate = deliveryDate, loadTime = loadTime, unloadTime = unloadTime, destination = destination, deliveryMass = deliveryMass},
                new DeliveryDTO(){deliveryID = "testID5", deliveryDate = deliveryDate, loadTime = loadTime, unloadTime = unloadTime, destination = destination, deliveryMass = deliveryMass},
            };

            return deliveryList;
        }

        [Test]
        public void DefineDriverServiceConstrutor()
        {
            var mockRepository = new Mock<IDeliveryRepository>();
            var mockUnit = new Mock<IUnitOfWork>();

            var service = new DeliveryService(mockUnit.Object, mockRepository.Object);

            Assert.That(service, Is.Not.Null);
        }

        [Test]
        public async Task GetAllTest()
        {
            var expectedList = GetDeliveriesList();

            var mockUnit = new Mock<IUnitOfWork>();
            var mockRepository = new Mock<IDeliveryRepository>();

            var serviceMock = new Mock<DeliveryService>(mockUnit.Object, mockRepository.Object);
            serviceMock.Setup(repo => repo.GetDeliveries()).ReturnsAsync(expectedList);

            var deliveryController = new DeliveryController(serviceMock.Object);
            var resultList = await deliveryController.GetAll();

            for (int i = 0; i < expectedList.Count; i++)
            {
                Assert.That(resultList.Value[i].deliveryID, Is.EqualTo(expectedList[i].deliveryID));
            }
        }

        [Test]
        public async Task GetByIDTest()
        {
            var delivery = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), destination, new DeliveryMass(deliveryMass));
            var deliveryExpected = delivery.toDeliveryDTO();
            var deliveryID = new DeliveryID(id);

            var mockUnit = new Mock<IUnitOfWork>();
            var mockRepository = new Mock<IDeliveryRepository>();

            var deliveryServiceMock = new Mock<DeliveryService>(mockUnit.Object, mockRepository.Object);
            deliveryServiceMock.Setup(repo => repo.GetDelivery(deliveryID)).ReturnsAsync(deliveryExpected);

            var deliveryController = new DeliveryController(deliveryServiceMock.Object);
            var aux = await deliveryController.GetByID(id);

            if (aux == null)
                Assert.Fail();
            else
            {
                var deliveryResult = (DeliveryDTO)(aux.Result as OkObjectResult).Value;

                Assert.AreEqual(deliveryExpected.deliveryID, deliveryResult.deliveryID);
            }
        }

        [Test]
        public async Task GetByPeriod()
        {
            var delivery = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), destination, new DeliveryMass(deliveryMass));
            var deliveryDTO = delivery.toDeliveryDTO();

            var dateTime1 = new DateTime(2023, 12, 10);
            var dateTime2 = new DateTime(2023, 12, 14);

            var listExpected = new List<DeliveryDTO>() { deliveryDTO };

            var mockUnit = new Mock<IUnitOfWork>();
            var mockRepository = new Mock<IDeliveryRepository>();

            var deliveryServiceMock = new Mock<DeliveryService>(mockUnit.Object, mockRepository.Object);
            deliveryServiceMock.Setup(repo => repo.GetByPeriod(dateTime1, dateTime2)).ReturnsAsync(listExpected);

            var deliveryController = new DeliveryController(deliveryServiceMock.Object);
            var aux = await deliveryController.GetByPeriod("10/12/2023,14/12/2023");

            if (aux == null)
                Assert.Fail();
            else
            {
                var listResult = (List<DeliveryDTO>)(aux.Result as OkObjectResult).Value;

                Assert.AreEqual(listExpected.First(), listResult.First());
            }
        }

        [Test]
        public async Task PostTest()
        {
            var delivery = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), destination, new DeliveryMass(deliveryMass));
            var deliveryExpected = delivery.toDeliveryDTO();

            var mockRepository = new Mock<IDeliveryRepository>();
            var mockUnit = new Mock<IUnitOfWork>();

            var deliveryServiceMock = new Mock<DeliveryService>(mockUnit.Object, mockRepository.Object);
            deliveryServiceMock.Setup(repo => repo.CreateDelivery(deliveryExpected)).ReturnsAsync(deliveryExpected);

            var deliveryController = new DeliveryController(deliveryServiceMock.Object);
            var aux = await deliveryController.Post(deliveryExpected);

            if (aux == null)
                Assert.Fail();
            else
            {
                var deliveryResult = (DeliveryDTO)(aux.Result as CreatedAtActionResult).Value;

                Assert.AreEqual(deliveryExpected.deliveryID, deliveryResult.deliveryID);
            }
        }


        [Test]
        public async Task PutTest()
        {
            var delivery = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), destination, new DeliveryMass(deliveryMass));

            var preChangeDto = delivery.toDeliveryDTO();
            var postChangeDto = delivery.toDeliveryDTO();

            float newUnloadTime = 3005;
            postChangeDto.unloadTime = newUnloadTime;

            var mockRepository = new Mock<IDeliveryRepository>();
            var mockUnit = new Mock<IUnitOfWork>();

            var deliveryServiceMock = new Mock<DeliveryService>(mockUnit.Object, mockRepository.Object);
            deliveryServiceMock.Setup(repo => repo.UpdateDelivery(preChangeDto)).ReturnsAsync(postChangeDto);

            var deliveryController = new DeliveryController(deliveryServiceMock.Object);
            var aux = await deliveryController.Patch(preChangeDto);

            if (aux == null)
                Assert.Fail();
            else
            {
                var deliveryResult = (DeliveryDTO)(aux.Result as OkObjectResult).Value;

                Assert.AreEqual(deliveryResult.unloadTime, newUnloadTime);
            }
        }

        [Test]
        public async Task DeleteTest()
        {
            var delivery = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), destination, new DeliveryMass(deliveryMass));
            var deliveryExpected = delivery.toDeliveryDTO();

            var mockRepository = new Mock<IDeliveryRepository>();
            var mockUnit = new Mock<IUnitOfWork>();

            var deliveryServiceMock = new Mock<DeliveryService>(mockUnit.Object, mockRepository.Object);
            deliveryServiceMock.Setup(repo => repo.DeleteDelivery(new DeliveryID(id))).ReturnsAsync(deliveryExpected);

            var deliveryController = new DeliveryController(deliveryServiceMock.Object);
            var aux = await deliveryController.Delete(id);

            if (aux == null)
                Assert.Fail();
            else
            {
                var deliveryResult = (DeliveryDTO)(aux.Result as OkObjectResult).Value;

                Assert.AreEqual(deliveryResult.deliveryID, id);
            }
        }

    }
}
