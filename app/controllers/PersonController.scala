package controllers

import java.util.Locale

import play.api._
import play.api.mvc._
import play.api.i18n._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.libs.json.{JsError, JsValue, Json, Writes}
import models._
import dal._

import scala.concurrent.{ExecutionContext, Future}
import javax.inject._

import play.api.http.{HeaderNames, MimeTypes}

class PersonController @Inject() (
                                   personRepository: PersonRepository,
                                   val messagesApi: MessagesApi
                                 )(implicit ec: ExecutionContext)
  extends Controller with I18nSupport {

  /**
    * The mapping for the person form.
    */
  val personForm: Form[PersonForm] = Form {
    mapping(
      "id" -> optional(longNumber),
      "name" -> nonEmptyText,
      "age" -> number.verifying(min(0), max(140))
    )(PersonForm.apply)(PersonForm.unapply)
  }

  /**
    * The index action.
    */
  def index = Action { implicit request =>
    Ok(views.html.index(personForm))
  }

  def newPerson = Action { implicit request =>
    Ok(views.html.person.personForm(personForm, None))
  }

  def toFilledForm(person: Person): Form[PersonForm] = personForm.fill(PersonForm(person.id, person.name, person.age))

  def show(id: Long) = Action.async { implicit request =>
    personRepository.findById(id).map {
      case Some(person) => {
        request.contentType match {
          case Some(MimeTypes.JSON) => {
            Ok(Json.toJson(person))
          }
          case _ => {
            Ok(views.html.person.show(person))
          }
        }
      }
      case None => Redirect(routes.PersonController.list())
    }
  }

  def edit(id: Long) = Action.async { implicit request =>
    personRepository.findById(id).map {
      case Some(person) => Ok(views.html.person.personForm(toFilledForm(person), Some(id)))
      case None => Redirect(routes.PersonController.list())
    }
  }

  def update(id: Long) = Action.async { implicit request =>
    request.contentType match {
      case Some(MimeTypes.JSON) => {
        val body: AnyContent = request.body
        val jsonBody: Option[JsValue] = body.asJson

        if (request.headers.keys.contains(HeaderNames.X_REQUESTED_WITH)) {
          jsonBody match {
            case Some(json) => {
              personRepository.findById(id).map(x => x.get).map { person =>
                // If successful, we simply redirect to the index page.
                personRepository.update(Person(Option[Long](id), (json \ "name").as[String], (json \ "age").as[Int]))
                Ok(Json.obj("status" ->"success"))
              }
            }
            case None => Future.successful(BadRequest(Json.obj("status" ->"error", "message" -> ("parse error"))))
          }
        } else {
          Future.successful(BadRequest(Json.obj("status" ->"error", "message" -> ("CSRF protection"))))
        }
      }
      case _ => {
        personForm.bindFromRequest.fold(
          errorForm => {
            Future.successful(Ok(views.html.person.personForm(errorForm, None)))
          },
          form => for (
            person  x.get);
        newPerson
        request.contentType match {
          case Some(MimeTypes.JSON) => {
            val body: AnyContent = request.body
            val jsonBody: Option[JsValue] = body.asJson

            if (request.headers.keys.contains(HeaderNames.X_REQUESTED_WITH)) {
              jsonBody match {
                case Some(json) => {
                  personRepository.findById(id).map(x => x.get).map { person =>
                    // If successful, we simply redirect to the index page.
                    personRepository.delete(person.id.get)
                    personRepository.create(Person((json \ "id").asOpt[Long], (json \ "name").as[String], (json \ "age").as[Int]))
                    Ok(Json.obj("status" ->"success"))
                  }
                }
                case None => Future.successful(BadRequest(Json.obj("status" ->"error", "message" -> ("parse error"))))
              }
            } else {
              Future.successful(BadRequest(Json.obj("status" ->"error", "message" -> ("CSRF protection"))))
            }
          }
          case _ => {
            personForm.bindFromRequest.fold(
              errorForm => {
                Future.successful(Ok(views.html.person.personForm(errorForm, None)))
              },
              form => for (
                person  x.get);
            newPerson
            // deleteはbodyを使わないかわりにCSRFチェック
            request.contentType match {
              case Some(MimeTypes.JSON) => {
                if (request.headers.keys.contains(HeaderNames.X_REQUESTED_WITH)) {
                  personRepository.findById(id.toLong).map { person =>
                    person match {
                      case Some(person) => {
                        personRepository.delete(person.id.get)
                        Ok(Json.obj("status" ->"success"))
                      }
                      case None => BadRequest(Json.obj("status" ->"error"))
                    }
                  }
                } else {
                  Future.successful(BadRequest(Json.obj("status" ->"error", "message" -> ("CSRF Protection"))))
                }
              }
              case _ => {
                personRepository.findById(id.toLong).map { person =>
                  person match {
                    case Some(person) => {
                      personRepository.delete(person.id.get)
                      Ok(Json.obj("status" ->"success"))
                    }
                    case None => BadRequest(Json.obj("status" ->"error"))
                  }
                }
              }
            }
          }

            implicit val PersonWrites = new Writes[Person] {
              override def writes(person: Person): JsValue = Json.obj(
                "id" -> person.id,
                "name" -> person.name,
                "age" -> person.age
              )
            }

            def list = Action.async { implicit request =>
              personRepository.list().map { people =>
                request.contentType match {
                  case Some(MimeTypes.JSON) => {
                    Ok(Json.toJson(people))
                  }
                  case _ => {
                    Ok(views.html.person.list(people))
                  }
                }
              }
            }

            /**
              * The add person action.
              *
              * This is asynchronous, since we're invoking the asynchronous methods on PersonRepository.
              */
            def create = Action.async { implicit request =>

              request.contentType match {
                case Some(MimeTypes.JSON) => {
                  val body: AnyContent = request.body
                  val jsonBody: Option[JsValue] = body.asJson

                  if (request.headers.keys.contains(HeaderNames.X_REQUESTED_WITH)) {
                    jsonBody.map { json =>
                      personRepository.create(Person(None, (json \ "name").as[String], (json \ "age").as[Int])).map { _ =>
                        // If successful, we simply redirect to the index page.
                        Ok(Json.obj("status" ->"success"))
                      }
                    }.getOrElse {
                      Future.successful(BadRequest(Json.obj("status" ->"error", "message" -> ("parse error"))))
                    }
                  } else {
                    Future.successful(BadRequest(Json.obj("status" ->"error", "message" -> ("CSRF protection"))))
                  }
                }
                case _ => {
                  personForm.bindFromRequest.fold(
                    errorForm => {
                      Future.successful(Ok(views.html.person.personForm(errorForm, None)))
                    },
                    person => {
                      personRepository.create(Person(None, person.name, person.age)).map { _ =>
                        // If successful, we simply redirect to the index page.
                        Redirect(routes.PersonController.list)
                      }
                    }
                  )
                }
              }
            }
        }
      }
    }
  }
        case class PersonForm(id: Option[Long], name: String, age: Int)
}