****************************************************************
*  顧客名　　　　：　サカタのタネ（株）殿　　　　　　　
*  業務名　　　　：　販売管理システム　　　　　　　　　
*  モジュール名　：　売上伝票ファイルコンバート
*  作成日／更新日：　2021/06/08
*  作成者／更新者：　INOUE
*  処理概要　　　：　売上伝票ファイルのサカタコードを変換する
*  更新履歴      ：
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            D365CNVC.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/06/08.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 売上伝票ファイル >>--*
     SELECT   SHTDENF   ASSIGN         DA-01-VI-SHTDENLC
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DEN-F277 DEN-F274
                                       DEN-F09  DEN-F01
                                       DEN-F02  DEN-F04
                                       DEN-F051 DEN-F07
                                       DEN-F112 DEN-F03
                        STATUS         SHTDENF-ST.
*----<< 商品変換ＴＢＬ >>--*
     SELECT   SUBTBLL1   ASSIGN         DA-01-VI-SUBTBLL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TBL-F01  TBL-F02
                        STATUS         SUBTBLL1-ST1.
*----<< 売上伝票ファイル変換結果 >>--*
     SELECT   SHTOUTF   ASSIGN         DA-01-VS-SHTOUTF
                        ORGANIZATION   SEQUENTIAL
                        ACCESS    MODE SEQUENTIAL
                        STATUS         SHTOUTF-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 売上伝票ファイル >>--*
 FD  SHTDENF            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
**----<< 商品変換ＴＢＬ >>--*
 FD  SUBTBLL1            LABEL RECORD   IS   STANDARD.
     COPY     SUBTBLL1   OF        XFDLIB
              JOINING   TBL       PREFIX.
*----<< 売上伝票ファイル変換結果 >>--*
 FD  SHTOUTF            LABEL     RECORD   IS   STANDARD.
     COPY     SHTOUTF   OF        XFDLIB
              JOINING   OUT       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(01).
     03  INV-FLG        PIC  9(01).
     03  ONL-FLG        PIC  9(01).
     03  TAN-ERR-FLG    PIC  9(01).
     03  TEI-FLG        PIC  9(01).
     03  TBL-INV-FLG    PIC  9(01).
 01  COUNTERS.
     03  IN-CNT         PIC  9(07).
     03  OUT-CNT        PIC  9(07).
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
     03  CNT            PIC  9(06).
 01  INDEXES.
     03  I              PIC  9(03).
 01  TANTO-WORK.
     03  TAN-OLD        PIC  9(02).
     03  TAN-NEW        PIC  9(02).
 01  WK-TANCD           PIC  9(02).
 01  WK-DEN-F45         PIC  9(08).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHTDENF-ST        PIC  X(02).
 01  SHTOUTF-ST        PIC  X(02).
 01  SUBTBLL1-ST.
     03  SUBTBLL1-ST1    PIC  X(02).
     03  SUBTBLL1-ST2    PIC  X(04).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  WK-NOUHIN          PIC  9(08).
 01  FILLER             REDEFINES      WK-NOUHIN.
     03  WK-YYYY        PIC  9(02).
     03  WK-YMD         PIC  9(06).
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
 01  WK-TOKCD           PIC  9(08).
 01  CYU-DATE           PIC  9(08).
 01  FILLER             REDEFINES      CYU-DATE.
     03  CYU-YY         PIC  9(04).
     03  CYU-MM         PIC  9(02).
     03  CYU-DD         PIC  9(02).
 01  NOU-DATE           PIC  9(08).
 01  FILLER             REDEFINES      NOU-DATE.
     03  NOU-YY         PIC  9(04).
     03  NOU-MM         PIC  9(02).
     03  NOU-DD         PIC  9(02).
 01  SYU-DATE           PIC  9(08).
 01  FILLER             REDEFINES      SYU-DATE.
     03  SYU-YY         PIC  9(04).
     03  SYU-MM         PIC  9(02).
     03  SYU-DD         PIC  9(02).
 01  CNV-YY             PIC  9(04)     VALUE  0.
 01  ACOS-MM            PIC  9(02)     VALUE  0.
 01  WK-BUMON           PIC  9(04)     VALUE  ZERO.
*----<< ﾌﾞﾚｲｸ ﾜｰｸ >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-F01    PIC  9(08) VALUE ZERO.
         05  NEW-F02    PIC  9(09) VALUE ZERO.
         05  NEW-F051   PIC  9(02) VALUE ZERO.
     03  OLD.
         05  OLD-F01    PIC  9(08) VALUE ZERO.
         05  OLD-F02    PIC  9(09) VALUE ZERO.
         05  OLD-F051   PIC  9(02) VALUE ZERO.
*
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02).
*--<< ACCEPT >>-*
 01  IN-TORICD          PIC  9(08) VALUE ZERO.
*--<< ｻﾌﾞﾙｰﾁﾝ ﾊﾟﾗﾒﾀ >>-*
 01  SSKTDTCK-PARA.
     03  SSKTDTCK-YMD        PIC  9(06).
     03  FILLER              REDEFINES SSKTDTCK-YMD.
         05  SSKTDTCK-Y      PIC  9(02).
         05  SSKTDTCK-M      PIC  9(02).
         05  SSKTDTCK-D      PIC  9(02).
     03  SSKTDTCK-RET        PIC  9(01).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
*LINKAGE                   SECTION.
*01  PARA-JKDATE           PIC 9(08).
*01  PARA-JKTIME           PIC 9(06).
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 売上伝票ファイル >>--*
 SHTDENF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### D365CNVC SHTDENF ERROR " SHTDENF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 商品変換ＴＢＬ >>--*
 TBL-ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SUBTBLL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### D365CNVC SUBTBLL1 ERROR " SUBTBLL1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 売上伝票ファイル変換結果 >>--*
 SHTOUTF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTOUTF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### D365CNVC SHTOUTF ERROR " SHTDENF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     GR-NO     =    99.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     SYS-TIME       TO   SYS-TIME2.
     DISPLAY  "*** D365CNVC START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD   TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     OPEN     I-O       SHTDENF.
     OPEN     INPUT     SUBTBLL1.
     OPEN     OUTPUT    SHTOUTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
     MOVE     0         TO   GR-NO.
*----<< ｼｽﾃﾑﾋﾂﾞｹﾍﾝｶﾝ >>-*
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-YYMD.
*
 100-INIT-00.
*----<< 取引先コード　ＡＣＣＥＰＴ　　 >>-*
     DISPLAY  NC"変換する取引先ＣＤを入力" UPON CONS.
     ACCEPT   IN-TORICD                    FROM CONS.
     IF       IN-TORICD NOT NUMERIC
              DISPLAY  NC"未入力です"      UPON CONS
               GO        TO    100-INIT-00
     END-IF.
 100-INIT-01.
*----<< 売上伝票ファイル　順ＲＥＡＤ　 >>-*
     PERFORM  900-DEN-READ.
     IF       NEW       =     HIGH-VALUE
              DISPLAY NC"対象データなし" UPON CONS
              MOVE      99    TO   GR-NO
              GO        TO    100-INIT-RTN-EXIT
     END-IF.
*
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
 200-MAIN-01.
*----<< 商品変換テーブル検索 >>-*
     MOVE     DEN-F01          TO   TBL-F01.
     MOVE     DEN-F25          TO   TBL-F02.
     READ     SUBTBLL1
              INVALID
                 DISPLAY NC"変換ＴＢＬなし" "取＝" DEN-F01
                                            "商＝" DEN-F25
                                            UPON CONS
                 GO            TO   200-MAIN-77
     END-READ.
 200-MAIN-02.
*----<< 更新対象比較 >>-*
     IF     ( DEN-F1411   =   TBL-F031 )  AND
            ( DEN-F1412   =   TBL-F032 )
              GO                     TO   200-MAIN-77
     ELSE
              MOVE     TBL-F031      TO   DEN-F1411
              MOVE     TBL-F032      TO   DEN-F1412
              MOVE     TBL-F19       TO   DEN-F32
     END-IF.
*
 200-MAIN-03.
*----<< レコード更新 >>-*
     MOVE     SPACE    TO   OUT-REC.
     INITIALIZE             OUT-REC.
     MOVE     DEN-REC  TO   OUT-REC.
     REWRITE  DEN-REC.
     WRITE    OUT-REC.
*
     ADD      1                TO   OUT-CNT.
*
 200-MAIN-77.
*----<< 売上伝票ファイル　順ＲＥＡＤ　 >>-*
     PERFORM  900-DEN-READ.
     IF       NEW       =     HIGH-VALUE
              MOVE      99    TO   GR-NO
              GO        TO    200-MAIN-RTN-EXIT
     END-IF.
*
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    SHTDENF.
     CLOSE    SUBTBLL1.
     CLOSE    SHTOUTF.
*
     DISPLAY  "+++ D365CNVC ﾖﾐｺﾐ  ｹﾝｽｳ = " IN-CNT " +++"
                                       UPON CONS.
     DISPLAY  "+++ D365CNVC ｺｳｼﾝ  ｹﾝｽｳ = " OUT-CNT " +++"
                                       UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** D365CNVC END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    売上伝票ファイル　 READ                      *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     READ     SHTDENF   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-DEN-READ-EXIT
     END-READ.
*
*----<< 売上データ作成（F277）判定 >>-*
 DEN010.
     IF       DEN-F277  =    9
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-DEN-READ-EXIT
     END-IF.
*
     ADD       1     TO   IN-CNT.
*
     IF  IN-CNT(4:4) = "0000" OR "5000"
         DISPLAY "READ-CNT = " IN-CNT UPON CONS
     END-IF.
*----<< 対象取引先コード　　判定　>>-*
 DEN020.
     IF       IN-TORICD NOT = ZERO
        IF    IN-TORICD NOT = DEN-F01
              GO        TO    900-DEN-READ
        END-IF
     END-IF.

*----<< 指定商品コード（F25）判定 >>-*
 DEN030.
     IF       DEN-F25   =    SPACE
              GO        TO    900-DEN-READ
     END-IF.
*
*
 900-DEN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
