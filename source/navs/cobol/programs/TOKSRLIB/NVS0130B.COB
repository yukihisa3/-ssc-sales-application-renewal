****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　DF365送受信実行ﾊﾟﾗﾒｰﾀﾌｧｲﾙ作成
*                            ＆履歴作成　　　　　　　　　　　　*
*    作成日／作成者　　　：　2020/02/24   ASS.II               *
*    処理内容　　　　　　：　実送受信を行うためのﾊﾟﾗﾒｰﾀを　　
*    　　　　　　　　　　　　出力する。　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NVS0130B.
 AUTHOR.               OONO.
 DATE-WRITTEN.         20/02/24.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*D365送受信実行パラメタファイル
     SELECT  D365PARA  ASSIGN    TO        DA-01-S-D365PARA
                       ORGANIZATION        SEQUENTIAL
                       ACCESS    MODE      SEQUENTIAL
                       FILE      STATUS    PAR-ST.
*
*NAVS送受信履歴管理ファイル
     SELECT   SNDRCVF  ASSIGN    TO        DA-01-VI-SNDRCVL3
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       RCV-F93
                                           RCV-F011
                       FILE  STATUS    IS  RCV-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
******************************************************************
*    D365送受信実行パラメタファイル
******************************************************************
 FD  D365PARA          LABEL     RECORD    IS   STANDARD.
                       COPY      D365PARA   OF   XFDLIB
                       JOINING   PAR       AS   PREFIX.
*
******************************************************************
*    NAVS送受信履歴管理ファイル
******************************************************************
 FD  SNDRCVF           LABEL     RECORD    IS   STANDARD.
                       COPY      SNDRCVF   OF   XFDLIB
                       JOINING   RCV       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  PAR-ST                   PIC  X(02).
     03  RCV-ST                   PIC  X(02).
*システム日付格納
 01  SYS-DATE                     PIC  9(06)  VALUE  ZERO.
*システム時間格納
 01  WK-TIME.
     03  WK-TIME-1                PIC  9(04)  VALUE  ZERO.
     03  WK-TIME-2                PIC  9(04)  VALUE  ZERO.
*時間編集領域
 01  WK-DATE.
     03  WK-YYYY                  PIC  9(04)  VALUE  ZERO.
     03  WK-MM                    PIC  9(02)  VALUE  ZERO.
     03  WK-DD                    PIC  9(02)  VALUE  ZERO.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  PAR-ERR           PIC N(15) VALUE
         NC"送受信実行パラメタファイル".
     03  RCV-ERR           PIC N(15) VALUE
         NC"送受信履歴管理ファイル".
*読込フラグ領域
 01  FLG-AREA.
     03  EDI-FLG           PIC  X(01) VALUE SPACE.
     03  KAI-FLG           PIC  X(01) VALUE SPACE.
     03  DAT-INV-FLG       PIC  X(03) VALUE SPACE.
     03  JYO-INV-FLG       PIC  X(03) VALUE SPACE.
*読込・書込カウント領域
 01  CNT-AREA.
     03  DAT-CNT           PIC  9(02) VALUE ZERO.
     03  GRP-CNT           PIC  9(02) VALUE ZERO.
     03  WRITE-CNT         PIC  9(02) VALUE ZERO.
*データ種別チェックテーブル
 01  DAT-CHK-TABLE.
     03  DAT-CHK           PIC  X(02) OCCURS  5.
*データ種別チェックテーブル
 01  INDEX-AREA.
     03  IX1               PIC  9(02) PACKED-DECIMAL.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*パラメタ取得
 01  LINK-RUNNO            PIC 9(07).
 01  LINK-SIJNO            PIC 9(10).
 01  LINK-SIJKBN           PIC X(01).
 01  LINK-RUNKBN           PIC X(01).
 01  LINK-DATSYB           PIC X(02).
 01  LINK-SDATE            PIC 9(08).
 01  LINK-STIME            PIC 9(06).
 01  LINK-RUNTNT           PIC X(02).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-RUNNO
                                           LINK-SIJNO
                                           LINK-SIJKBN
                                           LINK-RUNKBN
                                           LINK-DATSYB
                                           LINK-SDATE
                                           LINK-STIME
                                           LINK-RUNTNT.
**************************************************************
 DECLARATIVES.
 PAR-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE D365PARA.
     MOVE        PAR-ST    TO        E-ST.
     MOVE       "D365PARA" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     PAR-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 RCV-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SNDRCVF.
     MOVE        RCV-ST    TO        E-ST.
     MOVE        "SNDRCVF" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     RCV-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
**************************************
*     MOVE "3000001"    TO LINK-RUNNO
*     MOVE "4000000001" TO  LINK-SIJNO
*     MOVE "1"          TO  LINK-SIJKBN
*     MOVE "0"          TO  LINK-RUNKBN
*     MOVE "22"         TO  LINK-DATSYB
*     MOVE "20200304"   TO  LINK-SDATE
*     MOVE "120101"     TO  LINK-STIME
*     MOVE "33"         TO  LINK-RUNTNT
*
     DISPLAY "実行ＮＯ        =" LINK-RUNNO    UPON CONS
     DISPLAY "送受信指示_    ="  LINK-SIJNO   UPON CONS
     DISPLAY "送受信区分      ="  LINK-SIJKBN  UPON CONS
     DISPLAY "実行区分        ="  LINK-RUNKBN  UPON CONS
     DISPLAY "送受信データ種別="  LINK-DATSYB  UPON CONS
     DISPLAY "指示日付        ="  LINK-SDATE   UPON CONS
*****DISPLAY "指示時刻        ="  LINK-STIME
*****DISPLAY "実行担当者ＣＤ  ="  LINK-RUNTNT
**************************************
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC.
     PERFORM   END-SEC.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      I-O     SNDRCVF.
     OPEN      OUTPUT  D365PARA.
*システム日付取得／時刻取得
     ACCEPT    SYS-DATE    FROM    DATE.
     ACCEPT    WK-TIME     FROM    TIME.
*システム日付６桁→８桁変換（サブ日付チェック／変換）
 INIT010.
     MOVE      "3"         TO      LINK-IN-KBN.
     MOVE      SYS-DATE    TO      LINK-IN-YMD6.
     MOVE      ZERO        TO      LINK-IN-YMD8.
     MOVE      ZERO        TO      LINK-OUT-RET.
     MOVE      ZERO        TO      LINK-OUT-YMD.
     CALL     "SKYDTCKB"   USING   LINK-IN-KBN
                                   LINK-IN-YMD6
                                   LINK-IN-YMD8
                                   LINK-OUT-RET
                                   LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD TO     WK-DATE.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*送受信実行制御ファイル出力
     PERFORM  D365PARA-OUT-SEC.
*送受信実行制御ファイル出力
     PERFORM  SNDRCVF-OUT-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*
*ファイルのＯＰＥＮ
     CLOSE     SNDRCVF D365PARA.
*
 END-EXIT.
     EXIT.
****************************************************************
*             送受信実行制御ファイル出力              2.3      *
****************************************************************
 D365PARA-OUT-SEC      SECTION.
     MOVE     "D365PARA-OUT-SEC"   TO   S-NAME.
*送受信実行制御レコード編集
     MOVE      SPACE               TO   PAR-REC.
     INITIALIZE                         PAR-REC.
     MOVE      LINK-RUNNO          TO   PAR-F01.
     MOVE      LINK-SIJNO          TO   PAR-F02.
     MOVE      LINK-SIJKBN         TO   PAR-F03.
     MOVE      LINK-RUNKBN         TO   PAR-F04.
     MOVE      LINK-DATSYB         TO   PAR-F05.
     MOVE      LINK-SDATE          TO   PAR-F06.
     MOVE      LINK-STIME          TO   PAR-F07.
     MOVE      LINK-RUNTNT         TO   PAR-F08.
*送受信実行制御レコード出力
     WRITE     PAR-REC.
     ADD       1                    TO   WRITE-CNT.
*
 D365PARA-OUT-EXIT.
     EXIT.
****************************************************************
*             送受信実行制御ファイル出力              2.3      *
****************************************************************
 SNDRCVF-OUT-SEC      SECTION.
     MOVE     "SNDRCVF-OUT-SEC"   TO   S-NAME.
*送受信実行制御レコード編集
     MOVE      SPACE               TO   RCV-REC.
     INITIALIZE                         RCV-REC.
     MOVE      LINK-SIJNO          TO   RCV-F011.
     MOVE      LINK-SIJKBN         TO   RCV-F012.
     MOVE      LINK-RUNKBN         TO   RCV-F013.
     MOVE      LINK-DATSYB         TO   RCV-F014.
*    MOVE      DAT-F01             TO   RCV-F015.
     MOVE      LINK-SDATE          TO   RCV-F016.
     MOVE      LINK-STIME          TO   RCV-F017.
*    MOVE      DAT-F04             TO   RCV-F018.
     MOVE      LINK-RUNTNT         TO   RCV-F019.
*    MOVE      DAT-F06             TO   RCV-F01A.
*    MOVE      DAT-F07             TO   RCV-F01B.
*    MOVE      DAT-F08             TO   RCV-F01C.
     MOVE      LINK-RUNNO          TO   RCV-F01D.
     MOVE      LINK-RUNNO          TO   RCV-F93.
     MOVE      LINK-RUNTNT         TO   RCV-F94.
     MOVE      WK-DATE             TO   RCV-F95.
     MOVE      WK-TIME             TO   RCV-F96.
*    MOVE      LINK-RUNTNT         TO   RCV-F97.
*    MOVE      LINK-RUNTNT         TO   RCV-F98.
*    MOVE      LINK-RUNTNT         TO   RCV-F99.
*送受信実行制御レコード出力
     WRITE     RCV-REC.
     ADD       1                    TO   WRITE-CNT.
*
 SNDRCVF-OUT-EXIT.
     EXIT.
*****************<<  NVS0130B   END PROGRAM  >>******************
