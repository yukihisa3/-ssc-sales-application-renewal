# SJH8330B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SJH8330B.COB`

## ソースコード

```cobol
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
 PROGRAM-ID.            SJH8330B.
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
*ＪＣＡフォーマット（関東分）
     SELECT   JCAFILEA   ASSIGN    TO        DA-01-S-JCAFILEA
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCA1-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（仙台分）
     SELECT   JCAFILEB   ASSIGN    TO        DA-01-S-JCAFILEB
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCA2-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（北海道分）
     SELECT   JCAFILEC   ASSIGN    TO        DA-01-S-JCAFILEC
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCA3-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（対象外）
     SELECT   JCAFILED   ASSIGN    TO        DA-01-S-JCAFILED
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCA4-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
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
     03  EDI-02                  PIC  X(3148).
******************************************************************
*    ＪＣＡフォーマットファイル
******************************************************************
 FD  JCAFILEA            LABEL RECORD   IS   STANDARD.
*
 01  JCA1-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル
******************************************************************
 FD  JCAFILEB            LABEL RECORD   IS   STANDARD.
*
 01  JCA2-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル
******************************************************************
 FD  JCAFILEC            LABEL RECORD   IS   STANDARD.
*
 01  JCA3-REC.
     03  FILLER                   PIC X(128).
*
******************************************************************
*    ＪＣＡフォーマットファイル
******************************************************************
 FD  JCAFILED            LABEL RECORD   IS   STANDARD.
*
 01  JCA4-REC.
     03  FILLER                   PIC X(128).
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*ヘッダ情報格納領域
     COPY   HKJYRHED OF XFDLIB  JOINING   HED  AS   PREFIX.
*ヘッダ情報格納領域
     COPY   HKJYRMEI OF XFDLIB  JOINING   MEI  AS   PREFIX.
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
 01  WK-HED-F17              PIC  9(02)     VALUE  ZERO.
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
     03  WK-DEPD051         PIC  9(05)V9.
     03  WK-DEPD06          PIC  9(06)V99.
     03  WK-DEPD07          PIC  9(08).
     03  WK-DEPD08          PIC  9(06).
     03  WK-DEPD09          PIC  9(08).
     03  WK-DEPD10          PIC  X(13).
     03  WK-DEPD11          PIC  X(23).
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
     03  JCA1-STATUS       PIC  X(02).
     03  JCA2-STATUS       PIC  X(02).
     03  JCA3-STATUS       PIC  X(02).
     03  JCA4-STATUS       PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJH8330B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH8330B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH8330B".
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
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
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
                        PROCEDURE   JCAFILEA.
     MOVE      "JCAFILEA"    TO   AB-FILE.
     MOVE      JCA1-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILEB.
     MOVE      "JCAFILEB"    TO   AB-FILE.
     MOVE      JCA2-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILEC.
     MOVE      "JCAFILEC"    TO   AB-FILE.
     MOVE      JCA3-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILED.
     MOVE      "JCAFILED"    TO   AB-FILE.
     MOVE      JCA4-STATUS   TO   AB-STS.
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
     OPEN     INPUT     JEDICOS.
     OPEN     OUTPUT    JCAFILEA  JCAFILEB  JCAFILEC  JCAFILED.
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
***********JCA FORMAT 初期化
           MOVE      SPACE       TO   WK-DEPB-REC
           INITIALIZE                 WK-DEPB-REC
           MOVE      "H"         TO   WK-DEPB01
           MOVE      ZERO        TO   WK-DEPB02
           MOVE      HED-F03     TO   WK-DEPB03
           MOVE      HED-F05     TO   WK-DEPB04
           MOVE      HED-F064    TO   WK-DEPB05
           MOVE      HED-F27     TO   WK-DEPB061
           MOVE      ZERO        TO   WK-DEPB062
           MOVE      HED-F29     TO   WK-DEPB07
           MOVE      HED-F21     TO   WK-DEPB081
           MOVE      SPACE       TO   WK-DEPB082
           MOVE      HED-F22     TO   WK-DEPB09
           MOVE      HED-F14     TO   WK-DEPB101
           MOVE      SPACE       TO   WK-DEPB102
           MOVE      HED-F081    TO   WK-DEPB11
           MOVE      ZERO        TO   WK-DEPB12
           MOVE      ZERO        TO   WK-DEPB13
           MOVE      ZERO        TO   WK-DEPB14
           MOVE      HED-F10     TO   WK-DEPB15
           MOVE      SPACE       TO   WK-DEPB16
           MOVE      SPACE       TO   WK-DEPB17
           MOVE      HED-F17     TO   WK-HED-F17
           EVALUATE  WK-HED-F17
               WHEN  2
                     MOVE   WK-DEPB-REC TO JCA1-REC
                     WRITE  JCA1-REC
                     ADD    1    TO   JCA-CNT1
               WHEN  3
                     MOVE   WK-DEPB-REC TO JCA2-REC
                     WRITE  JCA2-REC
                     ADD    1    TO   JCA-CNT2
               WHEN  4
                     MOVE   WK-DEPB-REC TO JCA3-REC
                     WRITE  JCA3-REC
                     ADD    1    TO   JCA-CNT3
               WHEN  OTHER
                     MOVE   WK-DEPB-REC TO JCA4-REC
                     WRITE  JCA4-REC
                     ADD    1    TO   JCA-CNT4
           END-EVALUATE
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
***********ＪＣＡフォーマット項目セット
           MOVE      "L"         TO   WK-DEPD01
           MOVE      1           TO   WK-DEPD02
           MOVE      MEI-F04     TO   WK-DEPD031
           MOVE      SPACE       TO   WK-DEPD032
           MOVE      MEI-F07     TO   WK-DEPD04
           MOVE      MEI-F10     TO   WK-DEPD05
           MOVE      MEI-F122    TO   WK-DEPD051
           COMPUTE WK-DEPD06 = MEI-F15 / 100
           MOVE      MEI-F13     TO   WK-DEPD07
           MOVE      MEI-F16     TO   WK-DEPD08
           MOVE      MEI-F14     TO   WK-DEPD09
           MOVE      MEI-F02     TO   WK-DEPD10
           MOVE      SPACE       TO   WK-DEPD11
           EVALUATE  WK-HED-F17
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
***********ＪＣＡフォーマット初期化(明細）
           MOVE      SPACE       TO   WK-DEPT-REC
           INITIALIZE                 WK-DEPT-REC
           MOVE      "T"         TO   WK-DEPT01
           MOVE      ZERO        TO   WK-DEPT02
           MOVE      MEI-F13     TO   WK-DEPT03
           MOVE      MEI-F14     TO   WK-DEPT04
           MOVE      SPACE       TO   WK-DEPT05
           EVALUATE  WK-HED-F17
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
 END-SEC       SECTION.
*
     CLOSE     JEDICOS JCAFILEA JCAFILEB JCAFILEC JCAFILED.
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

```
