****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　出荷検品システム　　　　　　　　　
*    業務名　　　　　　　：　オンラインデータ抽出　　　　　　　
*    モジュール名　　　　：　出荷データ抽出（カインズ以外）　　
*    　　　　　　　　　　　　　　オンライン
*    作成日／更新日　　　：　2018/12/10
*    作成者／更新者　　　：　NAV
*    処理概要　　　　　　：　受け取ったパラメタのバッチNO.
*                            倉庫コードより該当のデータを
*                            抽出する。
*    作成日／更新日　　　：　2019/04/09
*    作成者／更新者　　　：　ＮＡＶ高橋_
*    更新履歴            ：　出荷指示数量＝０の場合、抽出対象
*                            としない。
*    作成日／更新日　　　：　2019/06/03
*    作成者／更新者　　　：　ＮＡＶ高橋_
*    更新履歴　　　　　　：　ホーマック／カーマのとき、商品名２
*                            の下５桁に発注種別区分、形状区分、
*                            発注単位区分をセットする様に変更
*    作成日／更新日　　　：　2024/03/21
*    作成者／更新者　　　：　ＮＡＶ高橋_
*    更新履歴　　　　　　：　アークランズ（３８７０９）の時、
*                            伝票番号重複防止のため、検品への　
*　                          連携は伝票番号の連番で連携に変更
*    作成日／更新日　　　：　2024/08/09
*    作成者／更新者　　　：　ＮＡＶ高橋_
*    更新履歴　　　　　　：　分類空白の時、ＡＬＬ９をセット
*    作成日／更新日　　　：　2024/08/09
*    作成者／更新者　　　：　ＮＡＶ高橋_
*    更新履歴　　　　　　：　欠品対応
*    作成日／更新日　　　：　2024/11/07
*    作成者／更新者　　　：　ＮＡＶ高橋_
*    更新履歴　　　　　　：　欠品対応の＄を停止する。
*　
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE0020B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/02/06.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YA        IS   YA
         YB-21     IS   YB-21
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENLA  ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F02   DEN-F04
                                            DEN-F051  DEN-F07
                                            DEN-F112  DEN-F03
                        FILE      STATUS    IS   DEN-STATUS.
*出荷指示データ
     SELECT   SNDSKDF   ASSIGN    TO        SNDSKDF
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    IS   SDE-STATUS.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE      STATUS    IS   TBL-STATUS.
*条件ファイル
     SELECT   JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*送信用件数Ｆ
     SELECT   SNDSKKF   ASSIGN         DA-01-S-SNDSKKF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         DEK-STATUS.
*プリントＦ
*    SELECT      PRINTF      ASSIGN    TO        LP-04-PRTF.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENLA
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    送信用売上伝票データ
******************************************************************
 FD  SNDSKDF            LABEL RECORD   IS   STANDARD.
     COPY     SNDSKDF   OF        XFDLIB
              JOINING   SDE       PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    条件ファイル
******************************************************************
 FD  JYOKEN1            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
******************************************************************
*    送信件数ファイル
******************************************************************
 FD  SNDSKKF            BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  DEK-REC.
     03  DEK-F01             PIC  9(08).
     03  DEK-F02             PIC  X(02).
******************************************************************
*    プリントファイル
******************************************************************
*FD  PRINTF.
*01  PRINT-REC                    PIC       X(200).
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*#2019/06/03 NAV ST
     COPY   KMSHIRED  OF  XFDLIB  JOINING  KMS  AS   PREFIX.
     COPY   HMSHIRED  OF  XFDLIB  JOINING  HMS  AS   PREFIX.
     COPY   DCMDENF   OF  XFDLIB  JOINING  DCM  AS   PREFIX.
*#2019/06/13 NAV ED
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WT-CNT                  PIC  9(08)     VALUE  ZERO.
 01  JYOKEN1-INV-FLG         PIC  X(03)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE.
         05  SYS-YY        PIC 9(02).
         05  SYS-MM        PIC 9(02).
         05  SYS-DD        PIC 9(02).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  SDE-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  DEK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NKE0020B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0020B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0020B".
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
*    数量編集
 01  WK-HEN                 PIC   9(09)V9(02).
 01  WK-HEN-R               REDEFINES   WK-HEN.
     03  WK-HEN-1           PIC   9(09).
     03  WK-HEN-2           PIC   9(02).
*    数量編集
 01  WK-HEN1.
     03  WK-HEN1-1          PIC   X(01).
     03  WK-HEN1-2          PIC   X(09).
     03  WK-HEN1-3          PIC   X(01).
     03  WK-HEN1-4          PIC   X(02).
*    日付編集
 01  WK-HEN-DATE.
     03  WK-HEN-DATE1       PIC   X(04).
     03  FILLER             PIC   X(01)  VALUE  "/".
     03  WK-HEN-DATE2       PIC   X(02).
     03  FILLER             PIC   X(01)  VALUE  "/".
     03  WK-HEN-DATE3       PIC   X(02).
*    時間編集
 01  WK-HEN-TIME.
     03  WK-HEN-TIME1       PIC   X(02).
     03  FILLER             PIC   X(01)  VALUE  ":".
     03  WK-HEN-TIME2       PIC   X(02).
*    トステムビバ伝票番号編集(店舗ＣＤを付加）
 01  WK-TOS-DEN.
     03  WK-TOS-DEN1        PIC   9(03)  VALUE  ZERO.
     03  WK-TOS-DEN2        PIC   9(06)  VALUE  ZERO.
*    日付変換１
 01  WK-HENKAN              PIC   9(08)  VALUE  ZERO.
 01  WK-HIDUKE.
     03  WK-HIDUKE1         PIC   9(04).
     03  WK-HIDUKE2         PIC   9(02).
     03  WK-HIDUKE3         PIC   9(02).
*    部門編集
 01  WK-BUMON               PIC   X(03).
 01  WK-BUMON-R             REDEFINES   WK-BUMON.
     03  WK-HEN-BUMON       PIC   9(03).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*    見出し行１
 01  MIDASHI1.
     03  FILLER                   PIC       X(36)  VALUE SPACE.
     03  FILLER                   PIC       N(19)  VALUE
       NC"【　ＯＮＬ　伝票ＤＴ振分件数リスト　】"
       CHARACTER   TYPE   IS   YB-21.
     03  FILLER                   PIC       X(16)  VALUE SPACE.
     03  FILLER                   PIC       X(05)  VALUE
         "DATE:".
     03  YY                       PIC       99.
     03  FILLER                   PIC       X(01)  VALUE
         ".".
     03  MM                       PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE
         ".".
     03  DD                       PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE SPACE.
     03  FILLER                   PIC       X(05)  VALUE
         "PAGE:".
     03  PEIJI                    PIC       ZZZ9.
*    見出し行２
 01  MIDASHI2           CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(50)  VALUE SPACE.
     03  FILLER                   PIC       N(17)  VALUE
       NC"伝票データ送信Ｆ　データ件数　＝　".
     03  KENSU                    PIC       ZZ,ZZZ,ZZ9.
*    線１
 01  SEN1               CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                   PIC       N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                   PIC       N(18)  VALUE
         NC"──────────────────".
*    線２
 01  SEN2.
     03  FILLER                   PIC       X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                   PIC       X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                   PIC       X(36)  VALUE
         "------------------------------------".
*
 01  WK-DEN-F112                  PIC       9(08)  VALUE  ZERO.
*
 01  WK-BRK-KEY.
     03  OLD-KEY.
         05  OLD-F46              PIC       9(08)  VALUE  ZERO.
         05  OLD-F47              PIC       9(04)  VALUE  ZERO.
         05  OLD-F01              PIC       9(08)  VALUE  ZERO.
         05  OLD-F48              PIC       X(02)  VALUE  SPACE.
         05  OLD-F02              PIC       9(09)  VALUE  ZERO.
         05  OLD-F07              PIC       9(05)  VALUE  ZERO.
         05  OLD-F112             PIC       9(08)  VALUE  ZERO.
     03  NEW-KEY.
         05  NEW-F46              PIC       9(08)  VALUE  ZERO.
         05  NEW-F47              PIC       9(04)  VALUE  ZERO.
         05  NEW-F01              PIC       9(08)  VALUE  ZERO.
         05  NEW-F48              PIC       X(02)  VALUE  SPACE.
         05  NEW-F02              PIC       9(09)  VALUE  ZERO.
         05  NEW-F07              PIC       9(05)  VALUE  ZERO.
         05  NEW-F112             PIC       9(08)  VALUE  ZERO.
 01  JYO-SAIBAN.
     03  WK-JYO-F04               PIC       9(09)  VALUE  ZERO.
     03  WK-JYO-F05               PIC       9(09)  VALUE  ZERO.
     03  WK-JYO-F06               PIC       9(09)  VALUE  ZERO.
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOHBI             PIC   9(08).
 01  PARA-BUMON             PIC   X(01).
 01  PARA-KENSU             PIC   9(07).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOHBI
                                       PARA-BUMON
                                       PARA-KENSU.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENLA.
     MOVE      "SHTDENLA"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDSKDF.
     MOVE      "SNDSKDF "   TO   AB-FILE.
     MOVE      SDE-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1 "   TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JYOKEN1.
     MOVE      "JYOKEN1 "   TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
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
     OPEN     INPUT     SHTDENLA  SHOTBL1.
*    OPEN     OUTPUT    SNDSKDF   SNDSKKF   PRINTF.
     OPEN     OUTPUT    SNDSKDF   SNDSKKF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
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
*３８７０９の時、条件Ｆ読込
     IF   PARA-TORICD  =  38709
          OPEN  INPUT  JYOKEN1
          MOVE  37              TO   JYO-F01
          MOVE  PARA-TORICD     TO   JYO-F02
          DISPLAY "#JYOKEN-KEY = " JYO-F01 ":" JYO-F02 UPON CONS
          PERFORM  JYOKEN1-READ-SEC
          IF   JYOKEN1-INV-FLG  =  SPACE
               MOVE  JYO-F04    TO   WK-JYO-F04
               MOVE  JYO-F05    TO   WK-JYO-F05
               MOVE  JYO-F06    TO   WK-JYO-F06
               DISPLAY NC"採番１＝"  WK-JYO-F04  UPON CONS
               DISPLAY NC"採番２＝"  WK-JYO-F05  UPON CONS
               DISPLAY NC"採番３＝"  WK-JYO-F06  UPON CONS
          ELSE
               DISPLAY NC"＃条件Ｆ取得エラー＃"  UPON CONS
               DISPLAY NC"＃取引先" "=" PARA-TORICD UPON CONS
               MOVE    4000     TO   PROGRAM-STATUS
               STOP  RUN
          END-IF
          CLOSE  JYOKEN1
     END-IF.
*
*
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     PARA-JDATE     TO   DEN-F46.
     MOVE     PARA-JTIME     TO   DEN-F47.
     MOVE     PARA-TORICD    TO   DEN-F01.
     MOVE     PARA-SOKO      TO   DEN-F48.
     START    SHTDENLA  KEY  >=   DEN-F46   DEN-F47
                                  DEN-F01   DEN-F48
                                  DEN-F02   DEN-F04
                                  DEN-F051  DEN-F07
                                  DEN-F112  DEN-F03
         INVALID   KEY
              MOVE      9    TO   END-FG
              GO   TO   INIT-EXIT
     END-START.

 INIT-010.
*
     PERFORM  SHTDENF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
     IF     ( PARA-JDATE     =    DEN-F46 ) AND
            ( PARA-JTIME     =    DEN-F47 ) AND
            ( PARA-TORICD    =    DEN-F01 )
         IF   PARA-SOKO      NOT =    SPACE
              IF   PARA-SOKO      =   DEN-F48
                   CONTINUE
              ELSE
                   IF   PARA-SOKO      <    DEN-F48
                        MOVE      9    TO   END-FG
                        GO   TO   MAIN-EXIT
                   ELSE
                        GO   TO   MAIN-010
                   END-IF
              END-IF
         END-IF
     ELSE
         MOVE      9         TO   END-FG
         GO        TO        MAIN-EXIT
     END-IF.
*
*出荷指示データ出力
     MOVE     SPACE          TO   SDE-REC.
     INITIALIZE                   SDE-REC.
*取引先ＣＤ
     MOVE     DEN-F01        TO   SDE-F01.
*伝票番号
     MOVE     DEN-F02        TO   SDE-F02.
*****トステムビバ伝票番号対応（店舗ＣＤ３桁＋伝票番号６桁）
     IF  DEN-F01  =  38709
*********MOVE DEN-F07        TO   WK-TOS-DEN1
*********MOVE DEN-F02        TO   WK-TOS-DEN2
*********MOVE WK-TOS-DEN     TO   SDE-F02
         IF  OLD-F48  =  NEW-F48
         AND OLD-F47  =  NEW-F47
         AND OLD-F01  =  NEW-F01
         AND OLD-F48  =  NEW-F48
         AND OLD-F02  =  NEW-F02
         AND OLD-F07  =  NEW-F07
         AND OLD-F112 =  NEW-F112
             MOVE  WK-JYO-F04      TO  SDE-F02
         ELSE
             ADD      1            TO  WK-JYO-F04
             IF  WK-JYO-F04  >  WK-JYO-F06
                 MOVE  WK-JYO-F05  TO  WK-JYO-F04
             END-IF
             MOVE  WK-JYO-F04      TO  SDE-F02
*************キーセット
             MOVE     DEN-F48          TO   OLD-F48
             MOVE     DEN-F47          TO   OLD-F47
             MOVE     DEN-F01          TO   OLD-F01
             MOVE     DEN-F48          TO   OLD-F48
             MOVE     DEN-F02          TO   OLD-F02
             MOVE     DEN-F07          TO   OLD-F07
             MOVE     DEN-F112         TO   OLD-F112
         END-IF
     END-IF.
*行番号
     MOVE     DEN-F03        TO   SDE-F03.
*店舗ＣＤ
     MOVE     DEN-F07        TO   SDE-F04.
*注文日
     MOVE     DEN-F111       TO   WK-HENKAN.
     MOVE     WK-HENKAN      TO   WK-HIDUKE.
     MOVE     WK-HIDUKE1     TO   WK-HEN-DATE1.
     MOVE     WK-HIDUKE2     TO   WK-HEN-DATE2.
     MOVE     WK-HIDUKE3     TO   WK-HEN-DATE3.
     MOVE     WK-HEN-DATE    TO   SDE-F05.
*納品日
     MOVE     DEN-F112       TO   WK-HENKAN.
     MOVE     WK-HENKAN      TO   WK-HIDUKE.
     MOVE     WK-HIDUKE1     TO   WK-HEN-DATE1.
     MOVE     WK-HIDUKE2     TO   WK-HEN-DATE2.
     MOVE     WK-HIDUKE3     TO   WK-HEN-DATE3.
     MOVE     WK-HEN-DATE    TO   SDE-F06.
*分類ＣＤ
     IF       PARA-BUMON = ZERO
              MOVE DEN-F12   TO   SDE-F07
     ELSE
              MOVE SPACE     TO   SDE-F07
     END-IF.
*#2024/08/09 NAV ST 部門＝空白は、ＨＨＴにてエラーになるため
     IF       SDE-F07  =  SPACE
              MOVE "9999"    TO   SDE-F07
     END-IF.
*#2024/08/09 NAV ED
     IF  DEN-F01 = 880 OR 882 OR 883 OR 1427 OR 14272 OR 14273
     OR  13938  OR  17137  OR  139381  OR  171371
     OR  1731   OR  7601
         MOVE SPACE          TO   DCM-REC
         INITIALIZE               DCM-REC
         MOVE DEN-REC        TO   DCM-REC
         MOVE SPACE          TO   SDE-F07
*********MOVE DCM-F12(3:1)   TO   SDE-F07(1:1)
         MOVE DCM-F12(1:3)   TO   WK-BUMON
         EVALUATE  WK-HEN-BUMON
             WHEN  1  MOVE "1"    TO   SDE-F07(1:1)
             WHEN  2  MOVE "2"    TO   SDE-F07(1:1)
             WHEN  3  MOVE "3"    TO   SDE-F07(1:1)
             WHEN  4  MOVE "4"    TO   SDE-F07(1:1)
             WHEN  5  MOVE "5"    TO   SDE-F07(1:1)
             WHEN  6  MOVE "6"    TO   SDE-F07(1:1)
             WHEN  7  MOVE "7"    TO   SDE-F07(1:1)
             WHEN  8  MOVE "8"    TO   SDE-F07(1:1)
             WHEN  9  MOVE "9"    TO   SDE-F07(1:1)
             WHEN 10  MOVE "A"    TO   SDE-F07(1:1)
             WHEN 11  MOVE "B"    TO   SDE-F07(1:1)
             WHEN 12  MOVE "C"    TO   SDE-F07(1:1)
             WHEN 13  MOVE "D"    TO   SDE-F07(1:1)
             WHEN 14  MOVE "E"    TO   SDE-F07(1:1)
             WHEN 15  MOVE "F"    TO   SDE-F07(1:1)
             WHEN 16  MOVE "G"    TO   SDE-F07(1:1)
             WHEN 17  MOVE "H"    TO   SDE-F07(1:1)
             WHEN OTHER MOVE "*"  TO   SDE-F07(1:1)
         END-EVALUATE
         MOVE DCM-M18        TO   SDE-F07(2:1)
         MOVE DCM-M16        TO   SDE-F07(3:1)
         MOVE DCM-M15        TO   SDE-F07(4:1)
         IF   DCM-M15  =  "C"
              MOVE  "3"      TO   SDE-F07(3:1)
         END-IF
******IF DEN-F25 = "4905980317567"
*        DISPLAY "DCM-F12 = " DCM-F12 "/" SDE-F07(1:1) UPON CONS
*        DISPLAY "DCM-M18 = " DCM-M18 "/" SDE-F07(2:1) UPON CONS
*        DISPLAY "DCM-M16 = " DCM-M16 "/" SDE-F07(3:1) UPON CONS
*        DISPLAY "DCM-M15 = " DCM-M15 "/" SDE-F07(4:1) UPON CONS
******END-IF
     END-IF.
*自社商品ＣＤ
     MOVE     DEN-F141       TO   SDE-F08.
*商品名称１
     MOVE     DEN-F1421      TO   SDE-F09.
*商品名称２
     MOVE     DEN-F1422      TO   SDE-F10.
*#2019/06/03 NAV ST
*下記取引先ＣＤのとき、商品名２の最終４桁に発注種別区分、形状
*区分、発注種別区分をセットする。
*****IF  DEN-F01 = 880 OR 882 OR 883 OR 1427 OR 14272 OR 14273
*        MOVE SPACE          TO   HMS-REC
*        INITIALIZE               HMS-REC
*        MOVE DEN-REC        TO   HMS-REC
*        MOVE HMS-A161       TO   SDE-F10(12:2)
*        MOVE HMS-A163       TO   SDE-F10(14:1)
*        MOVE HMS-A162       TO   SDE-F10(15:1)
*        IF   HMS-A162 = "P"
*             MOVE  ZERO     TO   SDE-F10(15:1)
*        ELSE
*             MOVE  "1"      TO   SDE-F10(15:1)
*        END-IF
*        IF   HMS-A162  = "C"
*             MOVE  "3"      TO   SDE-F10(14:1)
*        END-IF
*    END-IF.
*    IF  DEN-F01 = 13938  OR  17137  OR  139381  OR  171371
*        MOVE SPACE          TO   KMS-REC
*        INITIALIZE               KMS-REC
*        MOVE DEN-REC        TO   KMS-REC
*        MOVE KMS-A35        TO   SDE-F10(12:2)
*        MOVE KMS-A37        TO   SDE-F10(14:1)
*        MOVE KMS-A36        TO   SDE-F10(15:1)
*        IF   KMS-A36 = "P"
*             MOVE  ZERO     TO   SDE-F10(15:1)
*        ELSE
*             MOVE  "1"      TO   SDE-F10(15:1)
*        END-IF
*        IF   KMS-A36  = "C"
*             MOVE  "3"      TO   SDE-F10(14:1)
*        END-IF
*****END-IF.
*#2019/06/03 NAV ED
*数量
     INITIALIZE                   WK-HEN1.
*#2019/04/09 NAV ST
*指示数量＝０の場合は抽出対象としない
     IF       DEN-F15   =  ZERO
              GO             TO   MAIN-010
     END-IF.
*#2019/04/09 NAV ED
     IF       DEN-F15  <  ZERO
              MOVE   "-"     TO   WK-HEN1-1
     END-IF.
     MOVE     DEN-F15        TO   WK-HEN.
     MOVE     WK-HEN-1       TO   WK-HEN1-2.
     MOVE     "."            TO   WK-HEN1-3.
     MOVE     WK-HEN-2       TO   WK-HEN1-4.
     MOVE     WK-HEN1        TO   SDE-F11.
*原価単価
     INITIALIZE                   WK-HEN1.
     IF       DEN-F172  <  ZERO
              MOVE   "-"     TO   WK-HEN1-1
     END-IF.
     MOVE     DEN-F172       TO   WK-HEN.
     MOVE     WK-HEN-1       TO   WK-HEN1-2.
     MOVE     "."            TO   WK-HEN1-3.
     MOVE     WK-HEN-2       TO   WK-HEN1-4.
     MOVE     WK-HEN1        TO   SDE-F12.
*売価単価
     INITIALIZE                   WK-HEN1.
     IF       DEN-F173  <  ZERO
              MOVE   "-"     TO   WK-HEN1-1
     END-IF.
     MOVE     DEN-F173       TO   WK-HEN.
     MOVE     WK-HEN-1       TO   WK-HEN1-2.
     MOVE     "."            TO   WK-HEN1-3.
     MOVE     WK-HEN-2       TO   WK-HEN1-4.
     MOVE     WK-HEN1        TO   SDE-F13.
*指定商品ＣＤ（取引先商品ＣＤ）
     MOVE     DEN-F25        TO   SDE-F14.
*オンライン区分
     MOVE     DEN-F274       TO   SDE-F15.
*店舗名称（カナ）
     MOVE     DEN-F30        TO   SDE-F16.
*受信日
     MOVE     DEN-F46        TO   WK-HENKAN.
     MOVE     WK-HENKAN      TO   WK-HIDUKE.
     MOVE     WK-HIDUKE1     TO   WK-HEN-DATE1.
     MOVE     WK-HIDUKE2     TO   WK-HEN-DATE2.
     MOVE     WK-HIDUKE3     TO   WK-HEN-DATE3.
     MOVE     WK-HEN-DATE    TO   SDE-F17.
*受信時間
     MOVE     DEN-F471       TO   WK-HEN-TIME1.
     MOVE     DEN-F472       TO   WK-HEN-TIME2.
     MOVE     WK-HEN-TIME    TO   SDE-F18.
*振分倉庫ＣＤ
     MOVE     DEN-F48        TO   SDE-F19.
*_番
     MOVE     DEN-F49        TO   SDE-F20.
*訂正前数量
     INITIALIZE                   WK-HEN1.
     IF       DEN-F50  <  ZERO
              MOVE   "-"     TO   WK-HEN1-1
     END-IF.
     MOVE     DEN-F50        TO   WK-HEN.
     MOVE     WK-HEN-1       TO   WK-HEN1-2.
     MOVE     "."            TO   WK-HEN1-3.
     MOVE     WK-HEN-2       TO   WK-HEN1-4.
     MOVE     WK-HEN1        TO   SDE-F21.
*取引先固有エリア
*    MOVE     DEN-XI0004     TO   SDE-F22.
*  商品変換テーブル検索
     MOVE     DEN-F01        TO   TBL-F01.
     MOVE     DEN-F25        TO   TBL-F02.
     READ     SHOTBL1
       INVALID
              CONTINUE
       NOT  INVALID
              MOVE TBL-F08   TO   SDE-F20
**************サカタＣＤ再セット
              MOVE TBL-F03   TO   SDE-F08
     END-READ.
*#2024/08/09 NAV ST 欠品の場合、１桁目に＄をセットする。
*##2024/11/07 NAV ST 要望により停止
*****IF   DEN-F27D  =  ZERO
*         MOVE  "$"          TO   SDE-F20(1:1)
*****END-IF.
*##2024/11/07 NAV ED 要望により停止
*#2024/08/09 NAV ED
*改行コード
     MOVE     X"0D0A"        TO   SDE-F22.
*
     WRITE    SDE-REC.
     ADD      1              TO   WT-CNT.
*
 MAIN-010.
*
     PERFORM  SHTDENF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*    件数リスト発行
*    PERFORM LISTWT-SEC.
*件数Ｆ出力
     MOVE     SPACE               TO   DEK-REC.
     INITIALIZE                        DEK-REC.
*
     MOVE      WT-CNT             TO   DEK-F01 PARA-KENSU.
     MOVE      X"0D0A"            TO   DEK-F02.
*
     WRITE     DEK-REC.
*
     DISPLAY   NC"抽出件数＝" PARA-KENSU   UPON CONS.
*
*    CLOSE     SHTDENLA  SNDSKDF  SHOTBL1  PRINTF  SNDSKKF.
     CLOSE     SHTDENLA  SNDSKDF  SHOTBL1  SNDSKKF.
     IF   PARA-TORICD  =  38709
          OPEN  I-O  JYOKEN1
          MOVE  37              TO   JYO-F01
          MOVE  PARA-TORICD     TO   JYO-F02
          PERFORM  JYOKEN1-READ-SEC
          IF   JYOKEN1-INV-FLG  =  SPACE
               DISPLAY NC"戻し採番値＝"  WK-JYO-F04  UPON CONS
               MOVE  WK-JYO-F04 TO   JYO-F04
               REWRITE  JYO-REC
          ELSE
               DISPLAY NC"＃条件Ｆ採番値更新エラー＃"  UPON CONS
               DISPLAY NC"＃ナブへ連絡して下さい！＃"  UPON CONS
               MOVE    4000     TO   PROGRAM-STATUS
               STOP  RUN
          END-IF
          CLOSE  JYOKEN1
     END-IF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上伝票ファイル　ＲＥＡＤ　　　　　　　　　　　*
****************************************************************
 SHTDENF-READ-SEC      SECTION.
*
     MOVE "SHTDENF-READ-SEC"   TO   S-NAME.
*
     READ     SHTDENLA
              AT END
                 MOVE    9     TO   END-FG
                 GO            TO   SHTDENF-READ-EXIT
              NOT AT END
                 ADD     1     TO   RD-CNT
     END-READ.
*
     IF   PARA-NOHBI     NOT =    ZERO
          MOVE     DEN-F112    TO   WK-DEN-F112
          IF   PARA-NOHBI     =   WK-DEN-F112
                   CONTINUE
          ELSE
                   GO   TO   SHTDENF-READ-SEC
          END-IF
     END-IF
*****検品完了ＦＬＧチェック（検品済は読み飛ばし）
     IF       DEN-F279  =  1
              GO               TO   SHTDENF-READ-SEC
     END-IF.
*キーセット
     MOVE     DEN-F48          TO   NEW-F48.
     MOVE     DEN-F47          TO   NEW-F47.
     MOVE     DEN-F01          TO   NEW-F01.
     MOVE     DEN-F48          TO   NEW-F48.
     MOVE     DEN-F02          TO   NEW-F02.
     MOVE     DEN-F07          TO   NEW-F07.
     MOVE     DEN-F112         TO   NEW-F112.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　条件ファイル」ＲＥＡＤ　　　　　　　　　　　　　*
****************************************************************
 JYOKEN1-READ-SEC      SECTION.
*
     MOVE "JYOKEN1-READ-SEC"   TO   S-NAME.
*
     READ     JYOKEN1
              INVALID
                 MOVE    "INV" TO   JYOKEN1-INV-FLG
              NOT INVALID
                 MOVE    SPACE TO   JYOKEN1-INV-FLG
     END-READ.
*
 JYOKEN1-READ-EXIT.
     EXIT.
****************************************************************
*           リスト出力処理                          3.1.1      *
****************************************************************
*LISTWT-SEC   SECTION.
*
*    MOVE      SYS-YY         TO        YY.
*    MOVE      SYS-MM         TO        MM.
*    MOVE      SYS-DD         TO        DD.
*    MOVE      1              TO        PEIJI.
*    MOVE      WT-CNT         TO        KENSU.
*    WRITE     PRINT-REC      FROM      MIDASHI1 AFTER 2.
*    WRITE     PRINT-REC      FROM      SEN1     AFTER 2.
*    WRITE     PRINT-REC      FROM      MIDASHI2 AFTER 1.
*    WRITE     PRINT-REC      FROM      SEN1     AFTER 1.
*
*LISTWT-EXIT.
*    EXIT.
*-------------< PROGRAM END >------------------------------------*
