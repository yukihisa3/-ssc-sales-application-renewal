****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ホーマックＷＥＢＥＤＩシステム　　*
*    業務名　　　　　　　：　ホーマックＷＥＢＥＤＩシステム　　*
*    モジュール名　　　　：　納品予定データ送信形式変換        *
*    作成日／更新日　　　：　2006/09/15                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を出荷情報保存データより抽出する。*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY8305B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          04/07/22.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ホーマック納品予定データ
     SELECT   HCSYUKF   ASSIGN    TO        DA-01-VI-HCSYUKL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HCS-K01   HCS-K02
                                            HCS-K03   HCS-K04
                                            HCS-K08
                        FILE      STATUS    HCS-STATUS.
*ホーマック情報データ
     SELECT   HCJOHOF   ASSIGN    TO        DA-01-VI-HCJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HCJ-K01   HCJ-K02
                                            HCJ-K03   HCJ-K04
                                            HCJ-K05   HCJ-K06
                                            HCJ-K07   HCJ-K08
                        FILE      STATUS    HCJ-STATUS.
*ホーマック送信ファイル
     SELECT   SNDHOMAC  ASSIGN    TO        DA-01-S-SNDHOMAC
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SND-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    納品予定ファイル
******************************************************************
 FD  HCSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     HCSYUKF   OF        XFDLIB
              JOINING   HCS       PREFIX.
******************************************************************
*    情報データ
******************************************************************
 FD  HCJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     HCJOHOF   OF        XFDLIB
              JOINING   HCJ       PREFIX.
******************************************************************
*    ホーマック送信ファイル
******************************************************************
 FD  SNDHOMAC
                        BLOCK CONTAINS 1 RECORDS.
 01  SND-REC            PIC  X(3145).
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  HCJOHOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  SND-CNT             PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  SND-STATUS        PIC  X(02).
     03  HCS-STATUS        PIC  X(02).
     03  HCJ-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY8305B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8305B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8305B".
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
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOUDT.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HCSYUKF.
     MOVE      "HCSYUKL2"    TO   AB-FILE.
     MOVE      HCS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDHOMAC.
     MOVE      "SNDHOMAC"    TO   AB-FILE.
     MOVE      SND-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HCJOHOF.
     MOVE      "HCJOHOL1"    TO   AB-FILE.
     MOVE      HCJ-STATUS   TO   AB-STS.
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
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       HCSYUKF   HCJOHOF.
     OPEN     EXTEND    SNDHOMAC.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
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
*    ホーマック出荷情報スタート
     MOVE     SPACE          TO   HCS-REC.
     INITIALIZE                   HCS-REC.
     MOVE     PARA-JDATE     TO   HCS-K01.
     MOVE     PARA-JTIME     TO   HCS-K02.
     MOVE     PARA-TORICD    TO   HCS-K03.
     MOVE     PARA-SOKO      TO   HCS-K04.
     MOVE     PARA-NOUDT     TO   HCS-K08.
     START    HCSYUKF   KEY  >=   HCS-K01   HCS-K02
                                  HCS-K03   HCS-K04
                                  HCS-K08
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    ホーマック出荷情報保存データ読込み
     PERFORM HCSYUKF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 HCSYUKF-READ-SEC    SECTION.
*
     READ     HCSYUKF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  HCSYUKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
     IF    READ-CNT(6:3)  =  "000" OR "500"
           DISPLAY "READ-CNT = " READ-CNT  UPON CONS
     END-IF.
*    送信ＦＬＧのチェック
     IF       HCS-K11  =  "1"
              GO                 TO   HCSYUKF-READ-SEC
     END-IF.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  HCS-K01
     AND      PARA-JTIME  =  HCS-K02
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  HCSYUKF-READ-EXIT
     END-IF.
*倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
*             抽出条件のチェック
              IF       PARA-SOKO   =  HCS-K04
                       CONTINUE
              ELSE
                       GO        TO   HCSYUKF-READ-SEC
              END-IF
     END-IF.
*納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
*             抽出条件のチェック
              IF       PARA-NOUDT  =  HCS-K08
                       CONTINUE
              ELSE
                       GO        TO   HCSYUKF-READ-SEC
              END-IF
     END-IF.
*
 HCSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    送信ファイル出力
     MOVE     SPACE               TO   SND-REC.
     INITIALIZE                        SND-REC.
*ヘッダ部出力
     MOVE     "0"                 TO   HCS-A28(48:1).
     MOVE     HCS-K20             TO   SND-REC.
     WRITE    SND-REC.
     ADD      1                   TO   SND-CNT.
*明細部出力
*    送信ファイル出力
     MOVE     SPACE               TO   SND-REC.
     INITIALIZE                        SND-REC.
     MOVE     HCS-K21             TO   SND-REC.
     WRITE    SND-REC.
     ADD      1                   TO   SND-CNT.
*情報データにも書込み
     MOVE     SPACE               TO   HCJ-REC.
     INITIALIZE                        HCJ-REC.
     MOVE     HCS-K01             TO   HCJ-K01.
     MOVE     HCS-K02             TO   HCJ-K02.
     MOVE     HCS-K03             TO   HCJ-K03.
     MOVE     HCS-K04             TO   HCJ-K04.
     MOVE     HCS-K05             TO   HCJ-K05.
     MOVE     HCS-K06             TO   HCJ-K06.
     MOVE     HCS-K07             TO   HCJ-K07.
     MOVE     HCS-K08             TO   HCJ-K08.
     READ     HCJOHOF  INVALID
              MOVE     "INV"      TO   HCJOHOF-INV-FLG
              NOT  INVALID
              MOVE     SPACE      TO   HCJOHOF-INV-FLG
     END-READ.
*
     IF       HCJOHOF-INV-FLG = SPACE
              MOVE     SYS-DATEW           TO   HCJ-K12
              MOVE     "1"                 TO   HCJ-K11
              REWRITE  HCJ-REC
     END-IF.
*送信情報セット
     MOVE     SYS-DATEW           TO   HCS-K12.
     MOVE     "1"                 TO   HCS-K11.
     REWRITE  HCS-REC.
*
 MAIN010.
*    ホーマック出荷情報保存データ読込み
     PERFORM HCSYUKF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "ﾎｰﾏｯｸｼｭｯｶ   READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ﾎｰﾏｯｸｼｭｯｶ ｿｳｼﾝ   CNT = " SND-CNT    UPON CONS.
*
     CLOSE    HCSYUKF  HCJOHOF  SNDHOMAC.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
