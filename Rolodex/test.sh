#!/bin/sh

function get_contacts() {
	url="http://$1:$2/contacts"
	echo "curl $url"
	curl $url
	echo ""
	echo ""
}

function get_contacts_query() {
	url="http://$1:$2/contacts?$3=$4"
	echo "curl $url"
	curl $url
	echo ""
	echo ""
}

function get_contact() {
	url="http://$1:$2/contacts/$3"
	echo "curl $url"
	curl $url
	echo ""
	echo ""
}

function get_contact_query() {
	url="http://$1:$2/contacts/$3?$4=$5"
	echo "curl $url"
	curl $url
	echo ""
	echo ""
}
function post_contact() {
	url="http://$1:$2/contacts"
	echo "curl -H \"Content-Type: application/json\" -H \"Accept: application/json\" -X POST -d $3 $url"
	curl -H "Content-Type: application/json" -H "Accept: application/json" -X POST -d $3 $url
	echo ""
	echo ""
}

function put_contact() {
	url="http://$1:$2/contacts/$3"
	echo "curl -H \"Content-Type: application/json\" -H \"Accept: application/json\" -X PUT -d ${4} $url"
	curl -H "Content-Type: application/json" -H "Accept: application/json" -X PUT -d $4 $url
	echo ""
	echo ""
}

function delete_contact() {
	url="http://$1:$2/contacts/$3"
	echo "curl -X DELETE $url"
	curl -X DELETE $url
	echo ""
	echo ""
}

domain="localhost"
port=8000
contact1='{"firstName":"Sue","lastName":"Yamazaki","email":"sue.yamazaki@example.com","phoneNumber":"2065551212"}'
contact2='{"email":"joe.bob@example.com","phoneNumber":"1-206-555-1313","month":"May"}'

echo "[ALL POST SUCCESS]"
get_contacts $domain $port
post_contact $domain $port $contact1
get_contact $domain $port 1
get_contacts_query $domain $port "email" "sue.yamazaki@example"
get_contact_query $domain $port 1 "phoneNumber" "2065551212"
echo ""

echo "[ALL PUT SUCCESS]"
put_contact $domain $port 1 $contact2
get_contacts $domain $port
delete_contact $domain $port 1
get_contacts $domain $port
echo ""

failure1='{}'
failure2='{"email":"bad@email","phoneNumber":"12345679"}'
failure3='will not parse'

echo "[ALL PUT/POST FAILURE]"
get_contacts $domain $port
post_contact $domain $port $failure1
get_contacts $domain $port
post_contact $domain $port $failure2
get_contacts $domain $port
post_contact $domain $port $failure3
get_contacts $domain $port
