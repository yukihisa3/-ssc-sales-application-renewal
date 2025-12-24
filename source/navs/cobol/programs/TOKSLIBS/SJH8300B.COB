****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　ＪＥＤＩＣＯＳデータ変換　　　　　*
*    作成日／更新日　　　：　2006/09/13                        *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＪＥＤＩＣＯＳにて受信したデータ　*
*                            をＪＣＡフォーマットに変換する。　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJH8300B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          06/09/13.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   JEDICOS   ASSIGN    TO        DA-01-S-JEDICOS
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    EDI-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*基本情報データ
     SELECT   HCJOHOF   ASSIGN    TO        DA-01-VI-HCJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JOH-K01   JOH-K02
                                            JOH-K03   JOH-K04
                                            JOH-K05   JOH-K06
                                            JOH-K07   JOH-K08
                        FILE  STATUS   IS   JOH-STATUS.
*ＪＣＡフォーマット（関東分）
     SELECT   JCAFILE1   ASSIGN    TO        DA-01-S-JCAFILE1
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCA1-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（仙台分）
     SELECT   JCAFILE2   ASSIGN    TO        DA-01-S-JCAFILE2
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCA2-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（北海道分）
     SELECT   JCAFILE3   ASSIGN    TO        DA-01-S-JCAFILE3
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCA3-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（対象外）
     SELECT   JCAFILE4   ASSIGN    TO        DA-01-S-JCAFILE4
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCA4-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE  STATUS   IS   TOK-STATUS.
*ルート条件マスタ
     SELECT   JHMRUTL1  ASSIGN    TO        DA-01-VI-JHMRUTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RUT-F01   RUT-F02
                                            RUT-F03
                        FILE STATUS    IS   RUT-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　２５６　  ＢＦ＝　１
******************************************************************
 FD  JEDICOS
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  EDI-REC.
     03  EDI-01                  PIC  X(02).
     03  EDI-02                  PIC  X(3143).
******************************************************************
*    基本情報データ
******************************************************************
 FD  HCJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     HCJOHOF   OF        XFDLIB
              JOINING   JOH       PREFIX.
******************************************************************
*    ＪＣＡフォーマットファイル
******************************************************************
 FD  JCAFILE1            LABEL RECORD   IS   STANDARD.
*
 01  JCA1-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル
******************************************************************
 FD  JCAFILE2            LABEL RECORD   IS   STANDARD.
*
 01  JCA2-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル
******************************************************************
 FD  JCAFILE3            LABEL RECORD   IS   STANDARD.
*
 01  JCA3-REC.
     03  FILLER                   PIC X(128).
*
******************************************************************
*    ＪＣＡフォーマットファイル
******************************************************************
 FD  JCAFILE4            LABEL RECORD   IS   STANDARD.
*
 01  JCA4-REC.
     03  FILLER                   PIC X(128).
*
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
******************************************************************
*    ルート条件マスタ
******************************************************************
 FD  JHMRUTL1           LABEL RECORD   IS   STANDARD.
     COPY     JHMRUTF   OF        XFDLIB
              JOINING   RUT       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*ヘッダ情報格納領域
     COPY   HKHACHED OF XFDLIB  JOINING   HED  AS   PREFIX.
*ヘッダ情報格納領域
     COPY   HKHACMEI OF XFDLIB  JOINING   MEI  AS   PREFIX.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  JOH-CNT                 PIC  9(08)     VALUE  ZERO.
 01  JCA-CNT1                PIC  9(08)     VALUE  ZERO.
 01  JCA-CNT2                PIC  9(08)     VALUE  ZERO.
 01  JCA-CNT3                PIC  9(08)     VALUE  ZERO.
 01  JCA-CNT4                PIC  9(08)     VALUE  ZERO.
 01  TOKMS2-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  JHMRUTL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  WK-TOK-F81              PIC  X(02)     VALUE  SPACE.
 01  WK-JOH-F17              PIC  9(02)     VALUE  ZERO.
*
*ヘッドレコード退避ワーク
 01  WK-DEPB-REC.
     03  WK-DEPB01          PIC  X(01).
     03  WK-DEPB02          PIC  9(01).
     03  WK-DEPB03          PIC  9(07).
     03  WK-DEPB04          PIC  9(06).
     03  WK-DEPB05          PIC  9(06).
     03  WK-DEPB06.
         05  WK-DEPB061     PIC  9(06).
         05  WK-DEPB062     PIC  9(02).
     03  WK-DEPB07.
         05  WK-DEPB071     PIC  X(20).
         05  WK-DEPB072     PIC  X(20).
     03  WK-DEPB08.
         05  WK-DEPB081     PIC  9(04).
         05  WK-DEPB082     PIC  X(02).
     03  WK-DEPB09          PIC  X(15).
     03  WK-DEPB10.
         05  WK-DEPB101     PIC  9(03).
         05  WK-DEPB102     PIC  X(01).
     03  WK-DEPB11          PIC  9(02).
     03  WK-DEPB12          PIC  9(04).
     03  WK-DEPB13          PIC  9(02).
     03  WK-DEPB14          PIC  9(01).
     03  WK-DEPB15          PIC  9(01).
     03  WK-DEPB16          PIC  X(23).
     03  WK-DEPB17          PIC  X(01).

*    明細レコード退避ワーク
 01  WK-DEPD-REC.
     03  WK-DEPD01          PIC  X(01).
     03  WK-DEPD02          PIC  9(01).
     03  WK-DEPD03.
         05  WK-DEPD031     PIC  9(07).
         05  WK-DEPD032     PIC  X(01).
     03  WK-DEPD04.
         05  WK-DEPD041     PIC  X(20).
         05  WK-DEPD042     PIC  X(20).
     03  WK-DEPD05          PIC  9(05)V9.
     03  WK-DEPD06          PIC  9(06)V99.
     03  WK-DEPD07          PIC  9(08).
     03  WK-DEPD08          PIC  9(06).
     03  WK-DEPD09          PIC  9(08).
     03  WK-DEPD10          PIC  X(13).
     03  WK-DEPD11          PIC  X(29).
*    合計レコード退避ワーク
 01  WK-DEPT-REC.
     03  WK-DEPT01          PIC  X(01).
     03  WK-DEPT02          PIC  9(01).
     03  WK-DEPT03          PIC  9(08).
     03  WK-DEPT04          PIC  9(08).
     03  WK-DEPT05          PIC  X(110).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  EDI-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  JCA1-STATUS       PIC  X(02).
     03  JCA2-STATUS       PIC  X(02).
     03  JCA3-STATUS       PIC  X(02).
     03  JCA4-STATUS       PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  RUT-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJH8300B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH8300B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH8300B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE  PARA-JTIME.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JEDICOS.
     MOVE      "JEDICOS "   TO   AB-FILE.
     MOVE      EDI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HCJOHOF.
     MOVE      "HCJOHOF "   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILE1.
     MOVE      "JCAFILE1"    TO   AB-FILE.
     MOVE      JCA1-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILE2.
     MOVE      "JCAFILE2"    TO   AB-FILE.
     MOVE      JCA2-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILE3.
     MOVE      "JCAFILE3"    TO   AB-FILE.
     MOVE      JCA3-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILE4.
     MOVE      "JCAFILE4"    TO   AB-FILE.
     MOVE      JCA4-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2  "   TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHMRUTL1.
     MOVE      "JHMRUTL1"   TO   AB-FILE.
     MOVE      RUT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     JEDICOS   TOKMS2  JHMRUTL1.
     OPEN     I-O       HCJOHOF
     OPEN     OUTPUT    JCAFILE1  JCAFILE2  JCAFILE3  JCAFILE4.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT.
     MOVE     ZERO      TO        JOH-CNT JCA-CNT1 JCA-CNT2
                                  JCA-CNT3 JCA-CNT4.
     MOVE     SPACE     TO        WK-DEPB-REC.
     INITIALIZE                   WK-DEPB-REC.
     MOVE     SPACE     TO        WK-DEPD-REC.
     INITIALIZE                   WK-DEPD-REC.
     MOVE     SPACE     TO        WK-DEPT-REC.
     INITIALIZE                   WK-DEPT-REC.
     DISPLAY "PARA-JDATE = " PARA-JDATE UPON CONS.
     DISPLAY "PARA-JTIME = " PARA-JTIME UPON CONS.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
*
     PERFORM  JEDICOS-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
*    伝票ヘッダレコード
     IF    EDI-01  =   "HD"
***********ワークエリア初期化
           MOVE      SPACE       TO   HED-REC
           INITIALIZE                 HED-REC
***********ヘッダ情報→ワークにセット
           MOVE      EDI-REC     TO   HED-REC
***********基本情報データ作成
           MOVE      SPACE       TO   JOH-REC
           INITIALIZE                 JOH-REC
           MOVE      PARA-JDATE  TO   JOH-K01
           MOVE      PARA-JTIME  TO   JOH-K02
           MOVE      ZERO        TO   JOH-K03
           MOVE      HED-F21     TO   JOH-K05
           MOVE      HED-F03     TO   JOH-K06
           MOVE      HED-F06     TO   JOH-K08
           MOVE      HED-REC     TO   JOH-K20
***********JCA FORMAT 初期化
           MOVE      SPACE       TO   WK-DEPB-REC
           INITIALIZE                 WK-DEPB-REC
           MOVE      "H"         TO   WK-DEPB01
           MOVE      ZERO        TO   WK-DEPB02
           MOVE      HED-F03     TO   WK-DEPB03
           MOVE      HED-F05     TO   WK-DEPB04
           MOVE      HED-F06     TO   WK-DEPB05
           MOVE      HED-F27     TO   WK-DEPB061
           MOVE      ZERO        TO   WK-DEPB062
           MOVE      HED-F29     TO   WK-DEPB07
           MOVE      HED-F21     TO   WK-DEPB081
           MOVE      SPACE       TO   WK-DEPB082
           MOVE      HED-F22     TO   WK-DEPB09
           MOVE      HED-F14     TO   WK-DEPB101
           MOVE      SPACE       TO   WK-DEPB102
           MOVE      ZERO        TO   WK-DEPB11
           MOVE      ZERO        TO   WK-DEPB12
           MOVE      ZERO        TO   WK-DEPB13
***********MOVE      ZERO        TO   WK-DEPB14
           MOVE      HED-F09     TO   WK-DEPB14
           MOVE      HED-F10     TO   WK-DEPB15
           MOVE      SPACE       TO   WK-DEPB16
           MOVE      SPACE       TO   WK-DEPB17
***********MOVE      WK-DEPB-REC TO   JCA-REC
           MOVE      JOH-F17     TO   WK-JOH-F17
           EVALUATE  WK-JOH-F17
               WHEN  2
                     MOVE   WK-DEPB-REC TO JCA1-REC
                     WRITE  JCA1-REC
                     ADD    1    TO   JCA-CNT1
                     MOVE  883 TO   JOH-K03
               WHEN  3
                     MOVE   WK-DEPB-REC TO JCA2-REC
                     WRITE  JCA2-REC
                     ADD    1    TO   JCA-CNT2
                     MOVE  882   TO   JOH-K03
               WHEN  4
                     MOVE   WK-DEPB-REC TO JCA3-REC
                     WRITE  JCA3-REC
                     MOVE  880   TO   JOH-K03
                     ADD    1    TO   JCA-CNT3
               WHEN  OTHER
                     MOVE   WK-DEPB-REC TO JCA4-REC
                     WRITE  JCA4-REC
                     ADD    1    TO   JCA-CNT4
                     MOVE  880   TO   JOH-K03
           END-EVALUATE
           PERFORM  TOKMS2-READ-SEC
           IF TOKMS2-INV-FLG = "INV"
              DISPLAY "##ﾄﾘﾋｷｻｷ ｿｳｺ ｼｭﾄｸｴﾗｰ##" UPON CONS
              STOP  RUN
           ELSE
              MOVE  TOK-F81 TO  WK-TOK-F81
           END-IF
           PERFORM   JHMRUTL1-READ-SEC
***********WRITE     JCA-REC
***********ADD       1           TO   JCA-CNT
     END-IF.
*明細行
     IF    EDI-01  =  "DT"
***********ＪＣＡフォーマット初期化(明細）
           MOVE      SPACE       TO   WK-DEPD-REC
           INITIALIZE                 WK-DEPD-REC
***********ワークエリア初期化
           MOVE      SPACE       TO   MEI-REC
           INITIALIZE                 MEI-REC
***********ヘッダ情報→ワークにセット
           MOVE      EDI-REC     TO   MEI-REC
***********基本情報データ項目セット
           MOVE      MEI-F03     TO   JOH-K07
           MOVE      MEI-REC     TO   JOH-K21
***********欠品区分は０セット
           MOVE      ZERO        TO   JOH-M20
***********基本情報データ更新
           WRITE JOH-REC
           ADD       1           TO   JOH-CNT
***********ＪＣＡフォーマット項目セット
           MOVE      "L"         TO   WK-DEPD01
           MOVE      1           TO   WK-DEPD02
           MOVE      MEI-F04     TO   WK-DEPD031
           MOVE      SPACE       TO   WK-DEPD032
           MOVE      MEI-F07     TO   WK-DEPD04
           MOVE      MEI-F10     TO   WK-DEPD05
***********MOVE      MEI-F15     TO   WK-DEPD06
           COMPUTE WK-DEPD06 = MEI-F15 / 100
           MOVE      MEI-F13     TO   WK-DEPD07
           MOVE      MEI-F16     TO   WK-DEPD08
           MOVE      MEI-F14     TO   WK-DEPD09
           MOVE      MEI-F02     TO   WK-DEPD10
           MOVE      SPACE       TO   WK-DEPD11
***********MOVE      WK-DEPD-REC TO   JCA-REC
           EVALUATE  WK-JOH-F17
               WHEN  2
                     MOVE   WK-DEPD-REC TO JCA1-REC
                     WRITE  JCA1-REC
                     ADD    1    TO   JCA-CNT1
               WHEN  3
                     MOVE   WK-DEPD-REC TO JCA2-REC
                     WRITE  JCA2-REC
                     ADD    1    TO   JCA-CNT2
               WHEN  4
                     MOVE   WK-DEPD-REC TO JCA3-REC
                     WRITE  JCA3-REC
                     ADD    1    TO   JCA-CNT3
               WHEN  OTHER
                     MOVE   WK-DEPD-REC TO JCA4-REC
                     WRITE  JCA4-REC
                     ADD    1    TO   JCA-CNT4
           END-EVALUATE
***********WRITE     JCA-REC
***********ADD       1           TO   JCA-CNT
***********ＪＣＡフォーマット初期化(明細）
           MOVE      SPACE       TO   WK-DEPT-REC
           INITIALIZE                 WK-DEPT-REC
           MOVE      "T"         TO   WK-DEPT01
           MOVE      ZERO        TO   WK-DEPT02
           MOVE      MEI-F13     TO   WK-DEPT03
           MOVE      MEI-F14     TO   WK-DEPT04
           MOVE      SPACE       TO   WK-DEPT05
***********MOVE      WK-DEPT-REC TO   JCA-REC
           EVALUATE  WK-JOH-F17
               WHEN  2
                     MOVE   WK-DEPT-REC TO JCA1-REC
                     WRITE  JCA1-REC
                     ADD    1    TO   JCA-CNT1
               WHEN  3
                     MOVE   WK-DEPT-REC TO JCA2-REC
                     WRITE  JCA2-REC
                     ADD    1    TO   JCA-CNT2
               WHEN  4
                     MOVE   WK-DEPT-REC TO JCA3-REC
                     WRITE  JCA3-REC
                     ADD    1    TO   JCA-CNT3
               WHEN  OTHER
                     MOVE   WK-DEPT-REC TO JCA4-REC
                     WRITE  JCA4-REC
                     ADD    1    TO   JCA-CNT4
           END-EVALUATE
***********WRITE     JCA-REC
***********ADD       1           TO   JCA-CNT
     END-IF.
*
     PERFORM JEDICOS-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 JEDICOS-READ-SEC      SECTION.
*
     MOVE "JEDICOS-READ-SEC" TO       S-NAME.
*
     READ     JEDICOS
              AT END
              MOVE     9      TO    END-FG
              NOT AT END
              ADD      1      TO    RD-CNT
     END-READ.
*
     IF   RD-CNT(6:3) = "000" OR "500"
          DISPLAY "READ-CNT = " RD-CNT   UPON CONS
     END-IF.
*
 JEDICOS-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 TOKMS2-READ-SEC  SECTION.
*
     MOVE    SPACE         TO    TOK-REC
     INITIALIZE                  TOK-REC
*****MOVE    PARA-TOKCD    TO    TOK-F01
     MOVE    JOH-K03       TO    TOK-F01
     READ    TOKMS2
             INVALID
             MOVE  "INV"   TO    TOKMS2-INV-FLG
             NOT  INVALID
             MOVE  SPACE   TO    TOKMS2-INV-FLG
     END-READ
*
 TOKMS2-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 JHMRUTL1-READ-SEC  SECTION.
*
*****MOVE     PARA-TOKCD   TO        RUT-F01.
     MOVE     JOH-K03      TO        RUT-F01.
     MOVE     SPACE        TO        RUT-F02.
     MOVE     HED-F31(2:2) TO        RUT-F03.
     READ     JHMRUTL1
         INVALID
           MOVE  "INV"     TO        JHMRUTL1-INV-FLG
           MOVE WK-TOK-F81 TO        JOH-K04
         NOT INVALID
           MOVE  RUT-F05   TO        JOH-K04
     END-READ.
*
 JHMRUTL1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     CLOSE     JEDICOS  HCJOHOF  JCAFILE1  JCAFILE2 JCAFILE3.
*
     DISPLAY NC"＃全件　ＣＮＴ＝"  JOH-CNT   UPON CONS.
     DISPLAY NC"＃北海道ＣＮＴ＝"  JCA-CNT1  UPON CONS.
     DISPLAY NC"＃東北　ＣＮＴ＝"  JCA-CNT2  UPON CONS.
     DISPLAY NC"＃関東　ＣＮＴ＝"  JCA-CNT3  UPON CONS.
     DISPLAY NC"＃対象外ＣＮＴ＝"  JCA-CNT4  UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
