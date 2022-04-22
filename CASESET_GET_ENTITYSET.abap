  METHOD caseset_get_entityset.

    DATA ls_entityset   LIKE LINE OF et_entityset.

    DATA: lt_filters TYPE  /iwbep/t_mgw_select_option,
          ls_filter  TYPE  /iwbep/s_mgw_select_option,
          ls_so      TYPE                   /iwbep/s_cod_select_option,
          lt_orderby TYPE /iwbep/t_mgw_tech_order,
          ls_orderby TYPE /iwbep/s_mgw_tech_order.

    DATA lv_org_unit TYPE realo.
    DATA lv_guid_char TYPE string.

    TYPES: BEGIN OF ty_unit_requests_tt,
             guid      TYPE crmt_object_guid,
             object_id TYPE crmt_object_id_db,
             rating    TYPE numc3,
           END OF ty_unit_requests_tt.

    DATA lt_unit_requests TYPE STANDARD TABLE OF ty_unit_requests_tt.
    FIELD-SYMBOLS <ls_unit_requests> LIKE LINE OF lt_unit_requests.

    DATA lv_incident_rating TYPE numc3.
    DATA lv_selected_rating type numc3.

    DATA lt_org_hierarchy TYPE TABLE OF struc.
    FIELD-SYMBOLS <ls_org_hierarchy> LIKE LINE OF lt_org_hierarchy.

    DATA lv_selected_unit TYPE realo.
    DATA lv_selected_unit_pos TYPE int4.
    DATA lv_selected_unit_level TYPE int4.
    DATA lv_org_elements_count TYPE int4.
    DATA lv_loop_idx TYPE int4.

    DATA lv_processor_bp TYPE bu_partner.
    DATA lv_employer_bp TYPE bu_partner.
    DATA lv_employer_org_unit TYPE realo.

    DATA:
      api_object    TYPE REF TO cl_ags_crm_1o_api,
      api_object_sd TYPE REF TO cl_ags_crm_1o_api_sd,
      et_partner    TYPE crmt_partner_external_wrkt,
      ls_partner    TYPE crmt_partner_external_wrk.

    DATA lv_user_status TYPE crm_j_status.

    TYPES: BEGIN OF ty_selected_units_tt,
             objid TYPE realo,
             bp    TYPE bu_partner,
           END OF ty_selected_units_tt.

    DATA lt_selected_units TYPE STANDARD TABLE OF ty_selected_units_tt.
    FIELD-SYMBOLS <ls_selected_units> LIKE LINE OF lt_selected_units.
    DATA ls_selected_units LIKE LINE OF lt_selected_units.


    DATA lt_messages  TYPE  /salm/itsm_ssr_messaget.


    FIELD-SYMBOLS <ls_messages> LIKE LINE OF lt_messages.
    DATA lv_guid TYPE ags_sd_api_if_ws_obj_guid.
    DATA lv_chat_text TYPE bbpd_la_chatmsgstring.

    DATA: lv_datefrom TYPE dats,
          lv_dateto   TYPE dats.

    DATA lv_process_type TYPE crmt_process_type_db.
    DATA lv_root_org_unit TYPE realo.
    DATA lv_demo_mode TYPE char1.
    DATA lv_incident_rating_random TYPE int4.


    " Variables and constants for text processing

    DATA ls_proc_type TYPE crmc_proc_type.
    DATA ls_procedure TYPE comt_text_det_procedure.

    CONSTANTS: lc_object           TYPE comt_text_textobject VALUE 'CRM_ORDERH'.

    DATA: lt_struc_p          TYPE  comt_text_cust_struc2_tab,
          ls_struc_p          LIKE LINE OF lt_struc_p,
          lt_struc2_r         TYPE  comt_text_cust_struc2_tab,
          lt_textcom_p        TYPE comt_text_textcom_t,
          ls_textcom_p        LIKE LINE OF lt_textcom_p,
          lt_textdata         TYPE comt_text_textdata_t,
          ls_textdata_h_lines TYPE tline,
          ls_textdata_h       TYPE comt_text_textdata,
          lt_textdata_h       LIKE lt_textdata,
          lv_texts            TYPE string.



    " First checking filters and then keys
    lt_filters = io_tech_request_context->get_filter( )->get_filter_select_options( ).

   " Picking up organization unit

    READ TABLE lt_filters WITH TABLE KEY property = 'ORGUNIT' INTO ls_filter.

    LOOP AT ls_filter-select_options INTO ls_so.

      lv_selected_unit = ls_so-low.

    ENDLOOP.

   " Picking up rating filter

    IF ls_filter IS NOT INITIAL.
      CLEAR ls_filter.
    ENDIF. " IF ls_filter IS NOT INITIAL

    READ TABLE lt_filters WITH TABLE KEY property = 'RATING' INTO ls_filter.

     LOOP AT ls_filter-select_options INTO ls_so.

      lv_selected_rating = ls_so-low.

    ENDLOOP.

    IF ( lv_selected_unit IS INITIAL ).

      " Picking up organization unit

      READ TABLE it_key_tab INTO DATA(ls_scenario_key_tab) WITH KEY name = 'OrgUnit'.
      lv_selected_unit = ls_scenario_key_tab-value.

    ENDIF.

    " Picking up sorting order

    lt_orderby = io_tech_request_context->get_orderby( ).




    " Assigning dates


    IF line_exists( lt_filters[ property = 'DATEFROM' ] ).

      DATA(it_datefrom_struct) = lt_filters[ property = 'DATEFROM' ]-select_options.

      LOOP AT it_datefrom_struct ASSIGNING FIELD-SYMBOL(<it_datefrom_struct>).
        lv_datefrom = <it_datefrom_struct>-low.
      ENDLOOP.

    ENDIF.


    IF line_exists( lt_filters[ property = 'DATETO' ] ).

      DATA(it_dateto_struct) = lt_filters[ property = 'DATETO' ]-select_options.

      LOOP AT it_dateto_struct ASSIGNING FIELD-SYMBOL(<it_dateto_struct>).
        lv_dateto = <it_dateto_struct>-low.
      ENDLOOP.

    ENDIF.

    IF  ( lv_datefrom IS  INITIAL ) AND ( lv_dateto IS INITIAL ).

      DATA lv_year TYPE n LENGTH 4.
      lv_year = sy-datum+0(4).
      lv_year = lv_year - 2.
      CONCATENATE lv_year '01' '01' INTO lv_datefrom.
      lv_dateto = sy-datum.

    ENDIF.

    " Picking up root org unit node from parameters table

    SELECT SINGLE value FROM zcasemgmtparam INTO lv_root_org_unit
      WHERE param = 'REP_ROOT_ORG_UNIT'.

    " Receiving whole org structure of the system

    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype      = 'O'
        act_objid      = lv_root_org_unit
        act_wegid      = 'B002'
      TABLES
        result_struc   = lt_org_hierarchy
      EXCEPTIONS
        no_plvar_found = 1
        no_entry_found = 2
        OTHERS         = 3.

    " Calculating amount of elements in structure

    DESCRIBE TABLE lt_org_hierarchy LINES lv_org_elements_count.

    " Get selected org unit level

    READ TABLE lt_org_hierarchy ASSIGNING <ls_org_hierarchy> WITH KEY objid = lv_selected_unit.

    IF <ls_org_hierarchy> IS  ASSIGNED.

      lv_selected_unit_level = <ls_org_hierarchy>-level.
      lv_selected_unit_pos = sy-tabix.

      " Searching for all subsequent records and putting it to lt_selected_units

      ls_selected_units-objid = lv_selected_unit.
      SELECT SINGLE sobid INTO ls_selected_units-bp FROM hrp1001 WHERE objid = lv_selected_unit AND sclas = 'BP'.
      APPEND ls_selected_units TO lt_selected_units.

      lv_loop_idx = lv_org_elements_count - lv_selected_unit_pos.

      DO lv_loop_idx  TIMES.


        UNASSIGN <ls_org_hierarchy>.
        CLEAR ls_selected_units.

        " Switching to one level down
        lv_loop_idx = lv_loop_idx - 1.
        lv_selected_unit_pos = lv_selected_unit_pos + 1.


        READ TABLE lt_org_hierarchy ASSIGNING <ls_org_hierarchy> INDEX  lv_selected_unit_pos.

        IF ( <ls_org_hierarchy> IS ASSIGNED ) AND ( <ls_org_hierarchy>-level > lv_selected_unit_level ).

          ls_selected_units-objid = <ls_org_hierarchy>-objid.

          " Receiving BP for org structure

          SELECT SINGLE sobid INTO ls_selected_units-bp FROM hrp1001 WHERE objid = <ls_org_hierarchy>-objid AND sclas = 'BP'.

          APPEND ls_selected_units TO lt_selected_units.

        ELSEIF ( <ls_org_hierarchy> IS ASSIGNED ) AND ( <ls_org_hierarchy>-level <= lv_selected_unit_level ).
          EXIT.
        ENDIF.

      ENDDO. " DO lv_loop_idx  TIMES

      " Picking all incident and scanning it

      " Picking up process type for reporting from parameters table

      SELECT SINGLE value FROM zcasemgmtparam INTO lv_process_type
             WHERE param = 'REP_PROC_TYPE'.

      SELECT guid object_id FROM crmd_orderadm_h INTO TABLE lt_unit_requests
             WHERE process_type = lv_process_type AND posting_date GE lv_datefrom AND posting_date LE lv_dateto.

      LOOP AT lt_unit_requests ASSIGNING <ls_unit_requests>.


        CLEAR lv_employer_bp.
        CLEAR lv_processor_bp.
        CLEAR et_partner.
        CLEAR ls_partner.
        CLEAR ls_entityset.
        CLEAR lv_user_status.

        " ---------------- BP and status evaluation ----------------------

        CALL METHOD cl_ags_crm_1o_api=>get_instance
          EXPORTING
            iv_header_guid                = <ls_unit_requests>-guid
            iv_process_mode               = 'C'
            iv_process_type               = lv_process_type
          IMPORTING
            eo_instance                   = api_object
          EXCEPTIONS
            invalid_parameter_combination = 1
            error_occurred                = 2
            OTHERS                        = 3.

        " Searching for request status

        api_object->get_status( IMPORTING ev_user_status = lv_user_status ).

        " Skipping not completed requests

        IF lv_user_status <> 'E0008'.
          CONTINUE.
        ENDIF.

        " Processing closed incidents

        api_object_sd ?= api_object.
        api_object_sd->get_partners( IMPORTING et_partner = et_partner ).

        " Picking up processor SLFN0004 from partners

        LOOP AT et_partner INTO ls_partner.
          IF ls_partner-ref_partner_fct = 'SLFN0004'.
            lv_processor_bp = ls_partner-partner_no.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lv_processor_bp
              IMPORTING
                output = lv_processor_bp.

            " Search to which org structure processor belongs to

            " Receiving BP for processor support team
            SELECT SINGLE partner1 INTO lv_employer_bp FROM but050 WHERE partner2 = lv_processor_bp AND reltyp = 'BUR010'.

          ENDIF. " IF ls_partner-ref_partner_fct = 'SLFN0004'
        ENDLOOP. " LOOP AT et_partner INTO ls_partner

        " Processor cannot be found so we're taking support team SLFN0003 then

        IF lv_employer_bp IS INITIAL.
          LOOP AT et_partner INTO ls_partner.
            IF ls_partner-ref_partner_fct = 'SLFN0003'.
              lv_employer_bp = ls_partner-partner_no.
            ENDIF. " IF ls_partner-ref_partner_fct = 'SLFN0003'
          ENDLOOP. " LOOP AT et_partner INTO ls_partner

        ENDIF. " IF lv_processor_bp IS NOT INITIAL

        IF lv_employer_bp IS NOT INITIAL.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lv_employer_bp
            IMPORTING
              output = lv_employer_bp.


          READ TABLE lt_selected_units WITH KEY bp = lv_employer_bp TRANSPORTING NO FIELDS.

          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

        ELSE.
          CONTINUE.
        ENDIF.

        " ---------------- End of BP and status evaluation ------------------

        ls_entityset-objectid = <ls_unit_requests>-object_id.

        " Checking if we are in demo mode

        SELECT SINGLE value FROM zcasemgmtparam INTO lv_demo_mode
         WHERE param = 'REP_DEMO_MODE'.

        IF ( lv_demo_mode = 'X' ).

          CALL FUNCTION 'QF05_RANDOM_INTEGER'
            EXPORTING
              ran_int_max   = 5
              ran_int_min   = 1
            IMPORTING
              ran_int       = lv_incident_rating_random
            EXCEPTIONS
              invalid_input = 1
              OTHERS        = 2.

          lv_incident_rating = lv_incident_rating_random.

        ELSE.

          SELECT SINGLE zzfld000000 FROM crmd_customer_h INTO lv_incident_rating WHERE guid = <ls_unit_requests>-guid.

        ENDIF. " IF ( lv_demo_mode = 'X' )

        IF sy-subrc <> 0.
          lv_incident_rating = 0.
        ENDIF. " IF sy-subrc = 0


        " Filtering on selected rating

        IF ( lv_selected_rating > 0 ).

          IF ( lv_selected_rating <> lv_incident_rating ).
            CONTINUE.
          ENDIF. "IF ( lv_selected_rating <> lv_incident_rating )

        ENDIF. "IF ( lv_selected_rating > 0 )

        ls_entityset-rating = lv_incident_rating.
        ls_entityset-datefrom = lv_datefrom.
        ls_entityset-dateto = lv_dateto.

        " Adding URL

        DATA(lv_guid_hyphened) = cl_soap_wsrmb_helper=>convert_uuid_raw_to_hyphened( <ls_unit_requests>-guid ).

        lv_guid_char = lv_guid_hyphened.

        CONCATENATE 'https://sapapp1001.acbaca.local:50001/sap/bc/ui2/flp#Action-ZRESDISPINC&//IncidentSet(guid'
          '''' lv_guid_char  ''')' INTO ls_entityset-url.

        lv_guid = <ls_unit_requests>-guid.

        " Searching for a last comment

        CLEAR lv_process_type.
        CLEAR ls_proc_type.
        CLEAR ls_procedure.
        CLEAR lt_struc2_r.
        CLEAR lt_struc_p.
        CLEAR ls_struc_p.
        CLEAR ls_textcom_p.
        CLEAR lt_textcom_p.


        SELECT SINGLE process_type FROM crmd_orderadm_h INTO lv_process_type
        WHERE guid = lv_guid.
*


        CALL FUNCTION 'CRM_ORDER_PROC_TYPE_SELECT_CB'
          EXPORTING
            iv_process_type      = lv_process_type
          IMPORTING
            es_proc_type         = ls_proc_type
          EXCEPTIONS
            entry_not_found      = 1
            text_entry_not_found = 2
            OTHERS               = 3.

        IF sy-subrc = 0.

          ls_procedure = ls_proc_type-text_procedure.

          CALL FUNCTION 'COM_TEXT_CUST_I_PROTEXTID_READ'
            EXPORTING
              iv_object               = lc_object
              iv_procedure            = ls_procedure
            IMPORTING
              et_struc2_r             = lt_struc2_r
            CHANGING
              et_struc2               = lt_struc_p
            EXCEPTIONS
              textobject_missing      = 1
              textobject_not_found    = 2
              textprocedure_missing   = 3
              textprocedure_not_found = 4
              other_error             = 5
              OTHERS                  = 6.

          IF NOT lt_struc_p IS INITIAL.

            MOVE lc_object TO ls_textcom_p-stxh_key-tdobject.
            MOVE lv_guid TO ls_textcom_p-stxh_key-tdname.

            LOOP AT lt_struc_p INTO ls_struc_p.

              MOVE ls_struc_p-textid TO ls_textcom_p-stxh_key-tdid.
              APPEND ls_textcom_p TO lt_textcom_p.

            ENDLOOP.

            CALL FUNCTION 'COM_TEXT_READ_HIST_API'
              EXPORTING
                iv_object    = lc_object
                iv_procedure = ls_procedure
              IMPORTING
                et_textdata  = lt_textdata_h
              CHANGING
                it_textcom   = lt_textcom_p.

          ENDIF.   "  IF NOT lt_struc_p

          SORT lt_textdata_h BY stxh.

          LOOP AT lt_textdata_h INTO ls_textdata_h.

            LOOP AT ls_textdata_h-lines INTO ls_textdata_h_lines.

              IF ls_textdata_h_lines-tdline EQ '____________________'.

                SHIFT lv_texts LEFT DELETING LEADING space.
                ls_entityset-comments = lv_texts.
                CLEAR lv_texts.
                EXIT.

              ENDIF.

              IF ls_textdata_h_lines-tdline IS NOT INITIAL.
                CONCATENATE lv_texts  ls_textdata_h_lines-tdline '/' INTO lv_texts SEPARATED BY space.
              ENDIF. " IF ls_textdata_h_lines-tdline IS NOT INITIAL.


            ENDLOOP. " LOOP AT ls_textdata_h-lines INTO ls_textdata_h_lines


            EXIT.

          ENDLOOP. "LOOP AT lt_textdata_h INTO ls_textdata_h.

          CLEAR lt_textdata_h.
          CLEAR ls_textdata_h.

        ENDIF. "IF sy-subrc = 0

        APPEND ls_entityset TO et_entityset.

      ENDLOOP.

    ENDIF. " LOOP AT lt_unit_requests ASSIGNING <ls_unit_requests>

    " Sorting the output

    READ TABLE lt_orderby INTO ls_orderby INDEX 1.
    IF sy-subrc = 0.
      IF ls_orderby-property EQ 'RATING'.

        IF ls_orderby-order EQ 'desc'.

          SORT et_entityset BY rating DESCENDING.

        ENDIF. " if ls_orderby-order eq 'desc'


        IF ls_orderby-order EQ 'asc'.

          SORT et_entityset BY rating ASCENDING.
        ENDIF. " if ls_orderby-order eq 'asc'

      ENDIF.  " IF ls_orderby-property EQ 'RATING'

    ENDIF. " if sy-subrc = 0



  ENDMETHOD. " IF <ls_org_hierarchy> IS  ASSIGNED