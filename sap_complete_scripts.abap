*&---------------------------------------------------------------------*
*& Report ZSAP_COMPLETE_SCRIPTS
*&---------------------------------------------------------------------*
*& Description: Complete set of SAP ABAP scripts for common business processes
*&---------------------------------------------------------------------*
REPORT zsap_complete_scripts.

*----------------------------------------------------------------------*
* 1. FICO (Finance and Controlling) Scripts
*----------------------------------------------------------------------*
* Description:
* - create_accounting_doc: Creates accounting documents with header and line items
* - process_cost_center: Manages cost center data with validity checks
*----------------------------------------------------------------------*
CLASS lcl_fico_scripts DEFINITION.
  PUBLIC SECTION.
    METHODS:
      create_accounting_doc
        IMPORTING
          iv_bukrs TYPE bukrs
          iv_gjahr TYPE gjahr
          iv_monat TYPE monat
        RETURNING
          VALUE(rv_doc_number) TYPE bapiache09-doc_number,
      process_cost_center
        IMPORTING
          iv_kostl TYPE kostl
        RETURNING
          VALUE(rs_cost_center) TYPE csks.
ENDCLASS.

CLASS lcl_fico_scripts IMPLEMENTATION.
  METHOD create_accounting_doc.
    DATA: ls_header  TYPE bapiache09,
          lt_account TYPE TABLE OF bapiacgl09,
          lt_amount  TYPE TABLE OF bapiaccr09,
          lt_return  TYPE TABLE OF bapiret2.

    " Set document header
    ls_header-company_code = iv_bukrs.
    ls_header-fiscal_year  = iv_gjahr.
    ls_header-fiscal_period = iv_monat.
    ls_header-document_date = sy-datum.
    ls_header-posting_date = sy-datum.
    ls_header-document_type = 'SA'.
    ls_header-header_txt = 'Document created by ZSAP_COMPLETE_SCRIPTS'.

    " Create accounting document
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader = ls_header
      IMPORTING
        obj_type      = DATA(lv_obj_type)
        obj_key       = DATA(lv_obj_key)
        obj_sys       = DATA(lv_obj_sys)
      TABLES
        accountgl     = lt_account
        currencyamount = lt_amount
        return        = lt_return.

    " Check for errors
    READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      " Handle error
      MESSAGE e000(zz) WITH 'Error creating accounting document'.
    ELSE.
      rv_doc_number = lv_obj_key.
    ENDIF.
  ENDMETHOD.

  METHOD process_cost_center.
    " Read cost center data with validity check
    SELECT SINGLE * FROM csks
      INTO rs_cost_center
      WHERE kostl = iv_kostl
        AND datbi >= sy-datum
        AND datab <= sy-datum.

    IF sy-subrc <> 0.
      " Handle error
      MESSAGE e000(zz) WITH 'Cost center not found or invalid'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* 2. HR (Human Resources) Scripts
*----------------------------------------------------------------------*
* Description:
* - read_employee_data: Reads employee master data and organizational info
* - process_time_management: Handles time management and attendance data
*----------------------------------------------------------------------*
CLASS lcl_hr_scripts DEFINITION.
  PUBLIC SECTION.
    METHODS:
      read_employee_data
        IMPORTING
          iv_pernr TYPE pernr_d
        RETURNING
          VALUE(rs_employee) TYPE p0001,
      process_time_management
        IMPORTING
          iv_pernr TYPE pernr_d
          iv_date  TYPE datum
        RETURNING
          VALUE(rt_time_data) TYPE TABLE OF p2001.
ENDCLASS.

CLASS lcl_hr_scripts IMPLEMENTATION.
  METHOD read_employee_data.
    " Read employee master data
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr         = iv_pernr
        infty         = '0001'
        begda         = sy-datum
        endda         = sy-datum
      TABLES
        infty_tab     = DATA(lt_p0001)
      EXCEPTIONS
        infty_not_found = 1
        OTHERS         = 2.

    IF sy-subrc = 0.
      READ TABLE lt_p0001 INTO rs_employee INDEX 1.
    ELSE.
      " Handle error
      MESSAGE e000(zz) WITH 'Error reading employee data'.
    ENDIF.
  ENDMETHOD.

  METHOD process_time_management.
    " Read time data
    CALL FUNCTION 'HR_READ_TIME_DATA'
      EXPORTING
        pernr         = iv_pernr
        begda         = iv_date
        endda         = iv_date
      TABLES
        time_data     = rt_time_data
      EXCEPTIONS
        OTHERS        = 1.

    IF sy-subrc <> 0.
      " Handle error
      MESSAGE e000(zz) WITH 'Error reading time data'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* 3. SD (Sales and Distribution) Scripts
*----------------------------------------------------------------------*
* Description:
* - create_sales_order: Creates sales orders with header and items
* - process_billing: Handles billing document creation
*----------------------------------------------------------------------*
CLASS lcl_sd_scripts DEFINITION.
  PUBLIC SECTION.
    METHODS:
      create_sales_order
        IMPORTING
          iv_kunnr TYPE kunnr
        RETURNING
          VALUE(rv_vbeln) TYPE vbak-vbeln,
      process_billing
        IMPORTING
          iv_vbeln TYPE vbak-vbeln
        RETURNING
          VALUE(rv_vbeln) TYPE vbrk-vbeln.
ENDCLASS.

CLASS lcl_sd_scripts IMPLEMENTATION.
  METHOD create_sales_order.
    DATA: ls_header TYPE bapisdhd1,
          lt_items  TYPE TABLE OF bapisditm,
          lt_return TYPE TABLE OF bapiret2.

    " Set sales order header
    ls_header-doc_type = 'OR'.
    ls_header-sales_org = '1000'.
    ls_header-distr_chan = '10'.
    ls_header-division = '00'.
    ls_header-customer = iv_kunnr.

    " Create sales order
    CALL FUNCTION 'BAPI_SALESORDER_CREATE'
      EXPORTING
        sales_header_in = ls_header
      IMPORTING
        salesdocument  = rv_vbeln
      TABLES
        sales_items_in = lt_items
        return        = lt_return.

    " Check for errors
    READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      " Handle error
      MESSAGE e000(zz) WITH 'Error creating sales order'.
    ENDIF.
  ENDMETHOD.

  METHOD process_billing.
    DATA: lt_return TYPE TABLE OF bapiret2.

    " Create billing document
    CALL FUNCTION 'BAPI_BILLINGDOC_CREATE'
      EXPORTING
        salesdocument = iv_vbeln
      IMPORTING
        billingdocument = rv_vbeln
      TABLES
        return        = lt_return.

    " Check for errors
    READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      " Handle error
      MESSAGE e000(zz) WITH 'Error creating billing document'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* 4. MM (Materials Management) Scripts
*----------------------------------------------------------------------*
* Description:
* - create_purchase_order: Creates purchase orders with header and items
* - process_goods_receipt: Handles goods receipt processing
*----------------------------------------------------------------------*
CLASS lcl_mm_scripts DEFINITION.
  PUBLIC SECTION.
    METHODS:
      create_purchase_order
        IMPORTING
          iv_lifnr TYPE lifnr
        RETURNING
          VALUE(rv_ebeln) TYPE ekko-ebeln,
      process_goods_receipt
        IMPORTING
          iv_ebeln TYPE ekko-ebeln
        RETURNING
          VALUE(rv_mblnr) TYPE mseg-mblnr.
ENDCLASS.

CLASS lcl_mm_scripts IMPLEMENTATION.
  METHOD create_purchase_order.
    DATA: ls_header TYPE bapimepoheader,
          lt_items  TYPE TABLE OF bapimepoitem,
          lt_return TYPE TABLE OF bapiret2.

    " Set purchase order header
    ls_header-doc_type = 'NB'.
    ls_header-vendor = iv_lifnr.
    ls_header-comp_code = '1000'.
    ls_header-pur_group = '001'.

    " Create purchase order
    CALL FUNCTION 'BAPI_PO_CREATE'
      EXPORTING
        po_header     = ls_header
      IMPORTING
        ex_purchaseorder = rv_ebeln
      TABLES
        po_items      = lt_items
        return        = lt_return.

    " Check for errors
    READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      " Handle error
      MESSAGE e000(zz) WITH 'Error creating purchase order'.
    ENDIF.
  ENDMETHOD.

  METHOD process_goods_receipt.
    DATA: ls_header TYPE bapi2017_gm_head_01,
          lt_code   TYPE TABLE OF bapi2017_gm_code,
          lt_item   TYPE TABLE OF bapi2017_gm_item_create,
          lt_return TYPE TABLE OF bapiret2.

    " Set goods receipt header
    ls_header-pstng_date = sy-datum.
    ls_header-doc_date   = sy-datum.
    ls_header-ref_doc_no = iv_ebeln.

    " Create goods receipt
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header = ls_header
      IMPORTING
        goodsmvt_headret = DATA(ls_headret)
      TABLES
        goodsmvt_code  = lt_code
        goodsmvt_item  = lt_item
        return        = lt_return.

    " Check for errors
    READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      " Handle error
      MESSAGE e000(zz) WITH 'Error creating goods receipt'.
    ELSE.
      rv_mblnr = ls_headret-mat_doc.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* 5. Data Exchange Scripts
*----------------------------------------------------------------------*
* Description:
* - create_odata_service: Creates OData service for remote access
* - process_idoc: Handles IDOC processing
* - call_rest_service: Makes REST service calls
*----------------------------------------------------------------------*
CLASS lcl_data_exchange DEFINITION.
  PUBLIC SECTION.
    METHODS:
      create_odata_service
        IMPORTING
          iv_entity_name TYPE string
        RETURNING
          VALUE(ro_client) TYPE REF TO /iwbep/if_cp_client_proxy,
      process_idoc
        IMPORTING
          iv_idoc_number TYPE edidc-docnum
        RETURNING
          VALUE(rt_idoc_data) TYPE TABLE OF edidd,
      call_rest_service
        IMPORTING
          iv_url TYPE string
        RETURNING
          VALUE(rv_response) TYPE string.
ENDCLASS.

CLASS lcl_data_exchange IMPLEMENTATION.
  METHOD create_odata_service.
    DATA: lo_http_client TYPE REF TO if_http_client.

    " Create OData client
    TRY.
        lo_odata_client = /iwbep/if_cp_factory_remote~create_v2_remote_proxy(
          EXPORTING
            is_proxy_model_key      = VALUE #( repository_id       = 'DEFAULT'
                                             proxy_model_id      = 'Z_MATERIAL_SRV'
                                             proxy_model_version = '0001' )
            io_http_client         = lo_http_client
            iv_relative_service_root = '/sap/opu/odata/sap/Z_MATERIAL_SRV' ).
      CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
        " Handle error
        MESSAGE e000(zz) WITH 'Error creating OData service'.
    ENDTRY.
  ENDMETHOD.

  METHOD process_idoc.
    " Process IDOC
    CALL FUNCTION 'IDOC_READ_COMPLETELY'
      EXPORTING
        document_number         = iv_idoc_number
      TABLES
        int_edidd              = rt_idoc_data
      EXCEPTIONS
        document_not_exist     = 1
        document_number_invalid = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0.
      " Handle error
      MESSAGE e000(zz) WITH 'Error processing IDOC'.
    ENDIF.
  ENDMETHOD.

  METHOD call_rest_service.
    DATA: lo_http_client TYPE REF TO if_http_client.

    " Create HTTP client
    cl_http_client=>create_by_url(
      EXPORTING
        url                = iv_url
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS            = 4 ).

    IF sy-subrc <> 0.
      " Handle error
      MESSAGE e000(zz) WITH 'Error creating HTTP client'.
      RETURN.
    ENDIF.

    " Send request and get response
    lo_http_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state        = 2
        http_processing_failed    = 3
        OTHERS                   = 4 ).

    IF sy-subrc <> 0.
      " Handle error
      MESSAGE e000(zz) WITH 'Error sending HTTP request'.
      RETURN.
    ENDIF.

    lo_http_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state        = 2
        http_processing_failed    = 3
        OTHERS                   = 4 ).

    IF sy-subrc <> 0.
      " Handle error
      MESSAGE e000(zz) WITH 'Error receiving HTTP response'.
      RETURN.
    ENDIF.

    rv_response = lo_http_client->response->get_cdata( ).
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* 6. Form Processing Scripts
*----------------------------------------------------------------------*
* Description:
* - generate_adobe_pdf: Generates PDF documents using AdobeForms
* - print_sapscript_form: Prints forms using SAPScript
*----------------------------------------------------------------------*
CLASS lcl_form_processing DEFINITION.
  PUBLIC SECTION.
    METHODS:
      generate_adobe_pdf
        IMPORTING
          iv_document_number TYPE vbak-vbeln
        RETURNING
          VALUE(rv_pdf_data) TYPE xstring,
      print_sapscript_form
        IMPORTING
          iv_form_name TYPE tdsfname
        RETURNING
          VALUE(rv_success) TYPE abap_bool.
ENDCLASS.

CLASS lcl_form_processing IMPLEMENTATION.
  METHOD generate_adobe_pdf.
    DATA: lv_fm_name TYPE funcname.
    
    lv_fm_name = 'ZFM_SALES_ORDER_PDF'.
    
    CALL FUNCTION lv_fm_name
      EXPORTING
        iv_vbeln = iv_document_number
      IMPORTING
        ev_pdf   = rv_pdf_data
      EXCEPTIONS
        OTHERS   = 1.

    IF sy-subrc <> 0.
      " Handle error
      MESSAGE e000(zz) WITH 'Error generating PDF'.
    ENDIF.
  ENDMETHOD.

  METHOD print_sapscript_form.
    " Print using SAPScript
    CALL FUNCTION 'OPEN_FORM'
      EXPORTING
        form      = iv_form_name
      EXCEPTIONS
        OTHERS    = 1.

    IF sy-subrc <> 0.
      " Handle error
      MESSAGE e000(zz) WITH 'Error opening form'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'HEADER'
      EXCEPTIONS
        OTHERS  = 1.

    IF sy-subrc <> 0.
      " Handle error
      MESSAGE e000(zz) WITH 'Error writing form'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'CLOSE_FORM'
      EXCEPTIONS
        OTHERS  = 1.

    IF sy-subrc <> 0.
      " Handle error
      MESSAGE e000(zz) WITH 'Error closing form'.
      RETURN.
    ENDIF.

    rv_success = abap_true.
  ENDMETHOD.
ENDCLASS. 