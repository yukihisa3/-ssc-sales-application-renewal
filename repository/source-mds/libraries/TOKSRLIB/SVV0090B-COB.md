# SVV0090B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SVV0090B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＣＳＶ出力　　　　　　　　　　　　*
*    モジュール名　　　　：　商品名称マスタＣＳＶ出力　        *
*    作成日／作成者　　　：　2022/07/04 INOUE                  *
*    更新日／更新者　　　：　　　　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　商品名称マスタＣＳＶ出力を行う。  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SVV0090B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PRIMERGY6000.
 OBJECT-COMPUTER.       PRIMERGY6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         YA        IS   NIHONGO
         YB        IS   YB
         YB-21     IS   YB-21
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<  画面ファイル        >>******************************
     SELECT   DSPF      ASSIGN  TO   GS-DSPF
                        ORGANIZATION         IS  SEQUENTIAL
                        ACCESS  MODE         IS  SEQUENTIAL
                        SYMBOLIC DESTINATION IS  "DSP"
                        PROCESSING MODE      IS   DSP-PROC
                        GROUP                IS   DSP-GROUP
                        FORMAT               IS   DSP-FORMAT
                        SELECTED FUNCTION    IS   DSP-FUNC
                        FILE STATUS          IS   DSP-STATUS.
****<< 商品名称マスタ　　   >>******************************
     SELECT   SUBMEIF   ASSIGN  TO   DA-01-VI-SUBMEIL1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  SEQUENTIAL
                        RECORD  KEY          IS  MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE STATUS          IS  MEI-STATUS.
*****<< 条件ファイル >>**************************************
     SELECT   HJYOKEN   ASSIGN  TO   DA-01-VI-JYOKEN1
                        ORGANIZATION         IS  INDEXED
                        ACCESS    MODE       IS  DYNAMIC
                        RECORD    KEY        IS  JYO-F01
                                                 JYO-F02
                        FILE STATUS          IS  JYO-STATUS.
****<< 商品名称マスタ　ＣＳＶ >>******************************
     SELECT   SVVMEI2   ASSIGN  TO   DA-01-S-SVVMEI2
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   CSV-STATUS.
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  画面ファイル        >>******************************
 FD    DSPF.
 COPY     FVV00901           OF   XMDLIB.
****<< 商品名称マスタ　　　　 >>******************************
 FD    SUBMEIF.
       COPY     SUBMEIF    OF        XFDLIB
                JOINING   MEI  AS   PREFIX.
****<< 条件ファイル >>****************************************
 FD  HJYOKEN.
     COPY       HJYOKEN    OF        XFDLIB
                JOINING   JYO  AS   PREFIX.
****<< 商品名称マスタ　ＣＳＶ >>******************************
 FD    SVVMEI2.
       COPY     SVVMEI22   OF        XFDLIB
                JOINING  CSV   AS   PREFIX.
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ＣＳＶ タイトル行       ****
 COPY   SVVMEI21   OF        XFDLIB
        JOINING    CSV1      PREFIX.
****  画面制御項目            ****
 01  DSP-CONTROL.
     02 DSP-PROC             PIC  X(2).
     02 DSP-GROUP            PIC  X(8).
     02 DSP-FORMAT           PIC  X(8).
     02 DSP-STATUS           PIC  X(2).
     02 DSP-FUNC             PIC  X(4).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 CSV-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
     02 JYO-STATUS           PIC  X(2).
****  ワークエリア　          ****
*01  WK-AREA.
*    02 WK-STAN.
*        03  WK-SCATCD        PIC  X(05)  VALUE  SPACE.
*        03  WK-SJANCD        PIC  X(02)  VALUE  SPACE.
*        03  WK-STAN3         PIC  X(01)  VALUE  SPACE.
*    02 WK-ETAN.
*        03  WK-ECATCD        PIC  X(05)  VALUE  SPACE.
*        03  WK-EJANCD        PIC  X(02)  VALUE  SPACE.
*        03  WK-ETAN3         PIC  X(01)  VALUE  SPACE.
 01  WK-MEI-F041              PIC  9(7)V99 VALUE  ZERO.
 01  WK-MEI-F041-R  REDEFINES WK-MEI-F041.
     02 WK-MEI-F041-01           PIC  9(07).
     02 WK-MEI-F041-02           PIC  9(02).
 01  WK-CSV-M07.
     02 WK-CSV-M07-01            PIC  9(07).
     02 WK-CSV-M07-02            PIC  X(01)   VALUE  ".".
     02 WK-CSV-M07-03            PIC  9(02).
*
 01  WK-MEI-F042              PIC  9(7)V99 VALUE  ZERO.
 01  WK-MEI-F042-R  REDEFINES WK-MEI-F042.
     02 WK-MEI-F042-01           PIC  9(07).
     02 WK-MEI-F042-02           PIC  9(02).
 01  WK-CSV-M08.
     02 WK-CSV-M08-01            PIC  9(07).
     02 WK-CSV-M08-02            PIC  X(01)   VALUE  ".".
     02 WK-CSV-M08-03            PIC  9(02).
*
 01  WK-MEI-F043              PIC  9(7)V99 VALUE  ZERO.
 01  WK-MEI-F043-R  REDEFINES WK-MEI-F043.
     02 WK-MEI-F043-01           PIC  9(07).
     02 WK-MEI-F043-02           PIC  9(02).
 01  WK-CSV-M09.
     02 WK-CSV-M09-01            PIC  9(07).
     02 WK-CSV-M09-02            PIC  X(01)   VALUE  ".".
     02 WK-CSV-M09-03            PIC  9(02).
*
 01  WK-MEI-F07               PIC  9(7)V99 VALUE  ZERO.
 01  WK-MEI-F07-R  REDEFINES WK-MEI-F07.
     02 WK-MEI-F07-01            PIC  9(07).
     02 WK-MEI-F07-02            PIC  9(02).
 01  WK-CSV-M12.
     02 WK-CSV-M12-01            PIC  9(07).
     02 WK-CSV-M12-02            PIC  X(01)   VALUE  ".".
     02 WK-CSV-M12-03            PIC  9(02).
*
****  カウンタ                ****
 01  SUBMEIF-CNT              PIC  9(8)   VALUE  ZERO.
 01  SVVMEI2-CNT              PIC  9(8)   VALUE  ZERO.
****  フラグ                  ****
 01  DSP-FLG                 PIC  X(01)  VALUE  SPACE.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
**** エラーメッセージ         ****
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(30)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(30)  VALUE
            NC"開始・終了コードの関係に誤りがあります。".
     02  PMSG01              PIC N(20) VALUE
            NC"_取消　_終了".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SVV0090B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SVVMEI2.
     MOVE   "SVVMEI2  "        TO    ERR-FL-ID.
     MOVE    CSV-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  DSPF.
     MOVE   "DSPF    "        TO    ERR-FL-ID.
     MOVE    DSP-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HJYOKEN.
     MOVE   "HJYOKEN  "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SUBMEIF.
     MOVE   "SUBMEIF  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
 SVV0090B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 SVV0090B-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
     OPEN     I-O       DSPF.
     OPEN     INPUT     SUBMEIF.
     OPEN     INPUT     HJYOKEN.
     OPEN     OUTPUT    SVVMEI2.
     MOVE    "FVV00901"  TO     DSP-FORMAT.
     MOVE     SPACE     TO     FVV00901.
     MOVE     SPACE     TO     MSG1.
     MOVE     PMSG01    TO     MSG2.
     MOVE    "0"        TO     DSP-FLG.
     PERFORM  DSP-SEC.
     IF       END-FLG   NOT =  SPACE
              GO   TO   INIT-END
     END-IF.
**** MOVE     SSYOCD    TO     MEI-F011.
*    MOVE     SPACE     TO     MEI-F0121.
*    MOVE     SPACE     TO     MEI-F0122.
*    MOVE     SPACE     TO     MEI-F0123.
*    START    SUBMEIF   KEY  IS  >=  MEI-F011  MEI-F0121
*                                    MEI-F0122 MEI-F0123
*             INVALID   MOVE  "END"  TO  END-FLG
*                       GO   TO   INIT-END.
 010-INIT.
     READ     SUBMEIF
              AT END    MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END
     END-READ.
*    IF    (  MEI-F09         >    ECATCD   )  AND
*          (  MEI-D01         >    EJANCD   )  AND
*          (  MEI-F011        >    ESYOCD   )
*             MOVE  "END"    TO   END-FLG
*             GO TO  INIT-END
*    END-IF.
*****IF     ( SCATCD   <=  MEI-F09  ) AND
*           ( MEI-F09  <=  ECATCD   ) AND
*           ( SJANCD   <=  MEI-D01  ) AND
*           ( MEI-D01  <=  EJANCD   ) AND
*           ( SSYOCD   <=  MEI-F011 ) AND
*           ( MEI-F011 <=  ESYOCD   )
*             CONTINUE
*    ELSE
*             MOVE  "END"    TO   END-FLG
*             GO TO  INIT-END
*****END-IF.
     IF     ( SCATCD   <=  MEI-F09  ) AND
            ( MEI-F09  <=  ECATCD   ) AND
            ( SJANCD   <=  MEI-D01  ) AND
            ( MEI-D01  <=  EJANCD   ) AND
            ( SSYOCD   <=  MEI-F011 ) AND
            ( MEI-F011 <=  ESYOCD   )
              CONTINUE
     ELSE
              GO TO  010-INIT
     END-IF.
*
 020-INIT.
*    項目タイトルレコード出力
     MOVE     SPACE                   TO     CSV1-REC.
     MOVE     X"28"                   TO     CSV1-KS01
                                             CSV1-KS02
                                             CSV1-KS03
                                             CSV1-KS04
                                             CSV1-KS05
                                             CSV1-KS06
                                             CSV1-KS07
                                             CSV1-KS08
                                             CSV1-KS09
                                             CSV1-KS10
                                             CSV1-KS11
                                             CSV1-KS12
                                             CSV1-KS13
                                             CSV1-KS14
                                             CSV1-KS15
                                             CSV1-KS16
                                             CSV1-KS17
                                             CSV1-KS18
                                             CSV1-KS19
                                             CSV1-KS20
                                             CSV1-KS21
                                             CSV1-KS22
                                             CSV1-KS23
                                             CSV1-KS24
                                             CSV1-KS25.
     MOVE     X"29"                   TO     CSV1-KE01
                                             CSV1-KE02
                                             CSV1-KE03
                                             CSV1-KE04
                                             CSV1-KE05
                                             CSV1-KE06
                                             CSV1-KE07
                                             CSV1-KE08
                                             CSV1-KE09
                                             CSV1-KE10
                                             CSV1-KE11
                                             CSV1-KE12
                                             CSV1-KE13
                                             CSV1-KE14
                                             CSV1-KE15
                                             CSV1-KE16
                                             CSV1-KE17
                                             CSV1-KE18
                                             CSV1-KE19
                                             CSV1-KE20
                                             CSV1-KE21
                                             CSV1-KE22
                                             CSV1-KE23
                                             CSV1-KE24
                                             CSV1-KE25.
     MOVE     ","                     TO     CSV1-KK01
                                             CSV1-KK02
                                             CSV1-KK03
                                             CSV1-KK04
                                             CSV1-KK05
                                             CSV1-KK06
                                             CSV1-KK07
                                             CSV1-KK08
                                             CSV1-KK09
                                             CSV1-KK10
                                             CSV1-KK11
                                             CSV1-KK12
                                             CSV1-KK13
                                             CSV1-KK14
                                             CSV1-KK15
                                             CSV1-KK16
                                             CSV1-KK17
                                             CSV1-KK18
                                             CSV1-KK19
                                             CSV1-KK20
                                             CSV1-KK21
                                             CSV1-KK22
                                             CSV1-KK23
                                             CSV1-KK24
                                             CSV1-KK25.
     MOVE   NC"商品ＣＤ"             TO      CSV1-K01.
     MOVE   NC"品単ＣＤ"             TO      CSV1-K02.
     MOVE   NC"商品名１"             TO      CSV1-K03.
     MOVE   NC"商品名２"             TO      CSV1-K04.
     MOVE   NC"商品名カナ１"         TO      CSV1-K05.
     MOVE   NC"商品名カナ２"         TO      CSV1-K06.
     MOVE   NC"原価単価"             TO      CSV1-K07.
     MOVE   NC"卸単価"               TO      CSV1-K08.
     MOVE   NC"売価単価"             TO      CSV1-K09.
     MOVE   NC"仕入先ＣＤ"           TO      CSV1-K10.
     MOVE   NC"ＪＡＮＣＤ"           TO      CSV1-K11.
     MOVE   NC"入数"                 TO      CSV1-K12.
     MOVE   NC"廃盤区分"             TO      CSV1-K13.
     MOVE   NC"商品カテゴリ"         TO      CSV1-K14.
     MOVE   NC"商品カテゴリ名称"     TO      CSV1-K15.
     MOVE   NC"小売連携区分"         TO      CSV1-K16.
     MOVE   NC"振替商品区分"         TO      CSV1-K17.
     MOVE   NC"物流束区分"           TO      CSV1-K18
     MOVE   NC"２部用分類"           TO      CSV1-K19
     MOVE   NC"管理区分"             TO      CSV1-K20
     MOVE   NC"定番区分"             TO      CSV1-K21
     MOVE   NC"ストック_管理区分"   TO      CSV1-K22
     MOVE   NC"たねまるＪＡＮＣＤ"   TO      CSV1-K23
     MOVE   NC"たねまる商品ＣＤ"     TO      CSV1-K24
     MOVE   NC"オーダー区分"         TO      CSV1-K25.
     WRITE  CSV-REC                  FROM    CSV1-REC.
*
 INIT-END.
     EXIT.
************************************************************
*      _１     画面処理                                   *
************************************************************
 DSP-SEC                SECTION.
*画面表示
     MOVE     SPACE     TO     DSP-PROC.
     MOVE    "GPALL "   TO     DSP-GROUP.
     MOVE     HEN-DATE  TO     SDATE.
     MOVE     HEN-TIME  TO     STIME.
     WRITE    FVV00901.
*画面入力
     MOVE     SPACE     TO     MSG1.
     MOVE     "NE"      TO     DSP-PROC.
     IF   DSP-FLG   =   "0"
        MOVE    "GPHEAD"   TO     DSP-GROUP
     ELSE
        MOVE    "KAKNIN"   TO     DSP-GROUP
     END-IF.
     READ     DSPF.
*アテンション判定
     EVALUATE   DSP-FUNC
         WHEN   "F004"
                IF   DSP-FLG   =   "1"
                     MOVE   "0"       TO  DSP-FLG
                     MOVE   SPACE     TO  HEAD
                ELSE
                     MOVE   SPACE     TO  HEAD
                     MOVE   MSG-ERR1  TO  MSG1
                END-IF
                MOVE   "M"       TO  EDIT-OPTION  OF  SSYOCD
                MOVE   "M"       TO  EDIT-OPTION  OF  ESYOCD
                MOVE   "M"       TO  EDIT-OPTION  OF  SCATCD
                MOVE   "M"       TO  EDIT-OPTION  OF  SJANCD
                MOVE   "M"       TO  EDIT-OPTION  OF  ECATCD
                MOVE   "M"       TO  EDIT-OPTION  OF  EJANCD
                MOVE   " "       TO  EDIT-CURSOR  OF  SSYOCD
                MOVE   " "       TO  EDIT-CURSOR  OF  ESYOCD
                MOVE   " "       TO  EDIT-CURSOR  OF  SCATCD
                MOVE   " "       TO  EDIT-CURSOR  OF  SJANCD
                MOVE   " "       TO  EDIT-CURSOR  OF  ECATCD
                MOVE   " "       TO  EDIT-CURSOR  OF  EJANCD
                MOVE   SPACE     TO  MSG1
                GO TO  DSP-SEC
         WHEN   "F005"
                MOVE    "END"   TO    END-FLG
                MOVE    "END"   TO    PG-END
                GO TO  DSP-END
         WHEN   "E000"
                IF   SCATCD =    SPACE
                     MOVE  ALL " "    TO  SCATCD
                END-IF
                IF   ECATCD =    SPACE
                     MOVE  ALL "9"    TO  ECATCD
                END-IF
                IF   SJANCD =    SPACE
                     MOVE  ALL " "    TO  SJANCD
                END-IF
                IF   EJANCD =    SPACE
                     MOVE  ALL "9"    TO  EJANCD
                END-IF
                IF   SSYOCD =    SPACE
                     MOVE  "        " TO  SSYOCD
                END-IF
                IF   ESYOCD =    SPACE
                     MOVE  "99999999" TO  ESYOCD
                END-IF
*               IF   SCATCD  =    SPACE
*               AND  SJANCD  =    SPACE
*               AND  STAN3  =    SPACE
*                    MOVE  "     "    TO  SCATCD
*                    MOVE  "  "       TO  SJANCD
*                    MOVE  " "        TO  STAN3
*               END-IF
*               IF   ECATCD  =    SPACE
*               AND  EJANCD  =    SPACE
*               AND  ETAN3  =    SPACE
*                    MOVE  "99999"    TO  ECATCD
*                    MOVE  "99"       TO  EJANCD
*                    MOVE  "9"        TO  ETAN3
*               END-IF
*               MOVE        SCATCD     TO  WK-SCATCD
*               MOVE        SJANCD     TO  WK-SJANCD
*               MOVE        SSYOCD     TO  WK-SSYOCD
*               MOVE        ECATCD     TO  WK-ECATCD
*               MOVE        EJANCD     TO  WK-EJANCD
*               MOVE        ESYOCD     TO  WK-ESYOCD
                IF   SCATCD >   ECATCD
                     IF     MSG1        =   SPACE
                            MOVE   MSG-ERR2  TO  MSG1
                     END-IF
                     MOVE   "R"       TO  EDIT-OPTION  OF  SCATCD
                     MOVE   "R"       TO  EDIT-OPTION  OF  ECATCD
                     MOVE   "C"       TO  EDIT-CURSOR  OF  SCATCD
                     GO TO  DSP-SEC
                ELSE
                     MOVE   "M"       TO  EDIT-OPTION  OF  SCATCD
                     MOVE   "M"       TO  EDIT-OPTION  OF  ECATCD
                     MOVE   " "       TO  EDIT-CURSOR  OF  SCATCD
                     MOVE   " "       TO  EDIT-CURSOR  OF  ECATCD
                END-IF
                IF   SJANCD >   EJANCD
                     IF     MSG1        =   SPACE
                            MOVE   MSG-ERR2  TO  MSG1
                     END-IF
                     MOVE   "R"       TO  EDIT-OPTION  OF  SJANCD
                     MOVE   "R"       TO  EDIT-OPTION  OF  EJANCD
                     MOVE   "C"       TO  EDIT-CURSOR  OF  SJANCD
                     GO TO  DSP-SEC
                ELSE
                     MOVE   "M"       TO  EDIT-OPTION  OF  SJANCD
                     MOVE   "M"       TO  EDIT-OPTION  OF  EJANCD
                     MOVE   " "       TO  EDIT-CURSOR  OF  SJANCD
                     MOVE   " "       TO  EDIT-CURSOR  OF  EJANCD
                END-IF
                IF   SSYOCD >   ESYOCD
                     IF     MSG1        =   SPACE
                            MOVE   MSG-ERR2  TO  MSG1
                     END-IF
                     MOVE   "R"       TO  EDIT-OPTION  OF  SSYOCD
                     MOVE   "R"       TO  EDIT-OPTION  OF  ESYOCD
                     MOVE   "C"       TO  EDIT-CURSOR  OF  SSYOCD
                     GO TO  DSP-SEC
                ELSE
                     MOVE   "M"       TO  EDIT-OPTION  OF  SSYOCD
                     MOVE   "M"       TO  EDIT-OPTION  OF  ESYOCD
                     MOVE   " "       TO  EDIT-CURSOR  OF  SSYOCD
                     MOVE   " "       TO  EDIT-CURSOR  OF  ESYOCD
                END-IF
*               IF   WK-STAN >   WK-ETAN
*                    IF     MSG1        =   SPACE
*                           MOVE   MSG-ERR2  TO  MSG1
*                    END-IF
*                    MOVE   "R"       TO  EDIT-OPTION  OF  SCATCD
*                    MOVE   "R"       TO  EDIT-OPTION  OF  SJANCD
*                    MOVE   "R"       TO  EDIT-OPTION  OF  STAN3
*                    MOVE   "R"       TO  EDIT-OPTION  OF  ECATCD
*                    MOVE   "R"       TO  EDIT-OPTION  OF  EJANCD
*                    MOVE   "R"       TO  EDIT-OPTION  OF  ETAN3
*                    MOVE   "C"       TO  EDIT-CURSOR  OF  SCATCD
*                    GO TO  DSP-SEC
*               ELSE
*                    MOVE   "M"       TO  EDIT-OPTION  OF  SCATCD
*                    MOVE   "M"       TO  EDIT-OPTION  OF  SJANCD
*                    MOVE   "M"       TO  EDIT-OPTION  OF  STAN3
*                    MOVE   "M"       TO  EDIT-OPTION  OF  ECATCD
*                    MOVE   "M"       TO  EDIT-OPTION  OF  EJANCD
*                    MOVE   "M"       TO  EDIT-OPTION  OF  ETAN3
*                    MOVE   " "       TO  EDIT-CURSOR  OF  SCATCD
*                    MOVE   " "       TO  EDIT-CURSOR  OF  SJANCD
*                    MOVE   " "       TO  EDIT-CURSOR  OF  STAN3
*                    MOVE   " "       TO  EDIT-CURSOR  OF  ECATCD
*                    MOVE   " "       TO  EDIT-CURSOR  OF  EJANCD
*                    MOVE   " "       TO  EDIT-CURSOR  OF  ETAN3
*               END-IF
                IF   DSP-FLG   =   "0"
                     MOVE   "1"       TO  DSP-FLG
                     GO TO  DSP-SEC
                END-IF
         WHEN   OTHER
                MOVE   MSG-ERR1  TO  MSG1
                GO TO  DSP-SEC
     END-EVALUATE.
 DSP-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
*商品名称マスタカウントアップ
     ADD        1           TO      SUBMEIF-CNT.
     PERFORM  SVVMEI2-WRITE-SEC.
 010-MEI.
     READ     SUBMEIF
              AT END   MOVE  "END"  TO  END-FLG
              GO TO MAIN-END
     END-READ.
*    IF    (  MEI-F09       >       ECATCD  )  AND
*          (  MEI-D01       >       EJANCD  )  AND
*          (  MEI-F011      >       ESYOCD  )
*             MOVE  "END"  TO  END-FLG
*             GO TO MAIN-END
*    END-IF.
*    IF    (  MEI-F09       >=      SCATCD  ) AND
*             MEI-F09       <=      ECATCD
*             CONTINUE
*    END-IF.
*    IF    (  MEI-D01       >=      SJANCD  ) AND
*             MEI-D01       <=      EJANCD
*             CONTINUE
*    END-IF.
*    IF    (  MEI-F011      >=      SSYOCD  ) AND
*             MEI-F011      <=      ESYOCD
*             CONTINUE
*    ELSE
*             MOVE  "END"  TO  END-FLG
*             GO TO MAIN-END
*    END-IF.
     IF     ( SCATCD   <=  MEI-F09  ) AND
            ( MEI-F09  <=  ECATCD   ) AND
            ( SJANCD   <=  MEI-D01  ) AND
            ( MEI-D01  <=  EJANCD   ) AND
            ( SSYOCD   <=  MEI-F011 ) AND
            ( MEI-F011 <=  ESYOCD   )
              CONTINUE
     ELSE
              GO TO  010-MEI
     END-IF.
*
 MAIN-END.
     EXIT.
************************************************************
*      _１      商品名称マスタ　ＣＳＶ出力                *
************************************************************
 SVVMEI2-WRITE-SEC       SECTION.
*レコード初期化　　　　　　　　　　
     MOVE     SPACE         TO      CSV-REC.
     INITIALIZE                     CSV-REC.
*カンマセット
     MOVE     ","           TO      CSV-MK01 CSV-MK02 CSV-MK03
                                    CSV-MK04 CSV-MK05 CSV-MK06
                                    CSV-MK07 CSV-MK08 CSV-MK09
                                    CSV-MK10 CSV-MK11 CSV-MK12
                                    CSV-MK13 CSV-MK14 CSV-MK15
                                    CSV-MK16 CSV-MK17 CSV-MK18
                                    CSV-MK19 CSV-MK20 CSV-MK21
                                    CSV-MK22 CSV-MK23 CSV-MK24
                                    CSV-MK25.
     MOVE     X"28"         TO      CSV-MS15.
     MOVE     X"29"         TO      CSV-ME15.
*カウントアップ
     ADD       1            TO      SVVMEI2-CNT.
*商品コード
     MOVE      MEI-F011     TO      CSV-M01.
*品単コード
     MOVE      MEI-F012     TO      CSV-M02.
*商品名（日本語）_
     MOVE      MEI-F021     TO      CSV-M03.
*商品名（日本語）_
     MOVE      MEI-F022     TO      CSV-M04.
*商品名（カナ）_
     MOVE      MEI-F031     TO      CSV-M05.
*商品名（カナ）_
     MOVE      MEI-F032     TO      CSV-M06.
*原価単価
*----MOVE      MEI-F041     TO      CSV-M07.
     MOVE      MEI-F041        TO   WK-MEI-F041.
     MOVE      WK-MEI-F041-01  TO   WK-CSV-M07-01.
     MOVE      WK-MEI-F041-02  TO   WK-CSV-M07-03.
     MOVE      WK-CSV-M07      TO   CSV-M07.
*原単価
*----MOVE      MEI-F042        TO   CSV-M08.
     MOVE      MEI-F042        TO   WK-MEI-F042.
     MOVE      WK-MEI-F042-01  TO   WK-CSV-M08-01.
     MOVE      WK-MEI-F042-02  TO   WK-CSV-M08-03.
     MOVE      WK-CSV-M08      TO   CSV-M08.
*売単価　　　
*----MOVE      MEI-F043        TO   CSV-M09.
     MOVE      MEI-F043        TO   WK-MEI-F043.
     MOVE      WK-MEI-F043-01  TO   WK-CSV-M09-01.
     MOVE      WK-MEI-F043-02  TO   WK-CSV-M09-03.
     MOVE      WK-CSV-M09      TO   CSV-M09.
*仕入先コード
     MOVE      MEI-F05      TO      CSV-M10.
*ＪＡＮコード
     MOVE      MEI-F06      TO      CSV-M11.
*入数
*----MOVE      MEI-F07         TO   CSV-M12.
     MOVE      MEI-F07         TO   WK-MEI-F07.
     MOVE      WK-MEI-F07-01   TO   WK-CSV-M12-01.
     MOVE      WK-MEI-F07-02   TO   WK-CSV-M12-03.
     MOVE      WK-CSV-M12      TO   CSV-M12.
*廃盤区分
     MOVE      MEI-F08      TO      CSV-M13.
*商品カテゴリ
     MOVE      MEI-F09      TO      CSV-M14.
*商品カテゴリ名称
     MOVE      10           TO      JYO-F01.
     MOVE      MEI-F09      TO      JYO-F02.
     READ      HJYOKEN
         INVALID
               MOVE  ALL NC"＊"     TO      CSV-M15
         NOT INVALID
               MOVE  JYO-F03        TO      CSV-M15
     END-READ.
*小売連携区分
     MOVE      MEI-F10      TO      CSV-M16.
*振替商品区分
     MOVE      MEI-FIL1     TO      CSV-M17.
*物流束区分
     MOVE      MEI-F89      TO      CSV-M18.
*２部用分類
     MOVE      MEI-F90      TO      CSV-M19.
*管理区分
     MOVE      MEI-F91      TO      CSV-M20.
*定番区分
     MOVE      MEI-F95      TO      CSV-M21.
*ストック_管理区分
     MOVE      MEI-F97      TO      CSV-M22.
*たねまるＪＡＮＣＤ
     MOVE      MEI-D01      TO      CSV-M23.
*たねまる商品ＣＤ
     MOVE      MEI-D02      TO      CSV-M24.
*オーダー区分
     MOVE      MEI-D03      TO      CSV-M25.
*制御コード
     MOVE      X"28"        TO      CSV-MS03 CSV-MS04 CSV-MS15.
     MOVE      X"29"        TO      CSV-ME03 CSV-ME04 CSV-ME15.
*レコード出力
     WRITE     CSV-REC.

 SVVMEI2-WRITE-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF   SVVMEI2  SUBMEIF  HJYOKEN.

     IF    SUBMEIF-CNT NOT = ZERO
     DISPLAY  " SUBMEIF-READ-CNT = " SUBMEIF-CNT UPON CONS
     DISPLAY  " SVVMEI2-WRITE-CNT = " SVVMEI2-CNT UPON CONS
     ELSE
     DISPLAY  " SUBMEIF-READ-CNT = " SUBMEIF-CNT UPON CONS
     DISPLAY  " SVVMEI2-WRITE-CNT = " SVVMEI2-CNT UPON CONS
     DISPLAY  NC"＃＃出力対象無＃＃"  UPON CONS
     MOVE    4010      TO     PROGRAM-STATUS
     END-IF.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
