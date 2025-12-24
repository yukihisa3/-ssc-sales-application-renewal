****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩ　　　　　　　　　　*
*    業務名　　　　　　　：　新ＥＤＩオンラインシステム　　　　*
*    モジュール名　　　　：　物流会社連携データ抽出　　　　　  *
*    作成日／更新日　　　：　2011/11/29                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　パラメータを受取り、ナフコ基本情報*
*                            ファイルよりデータをテクノス連携　*
*                            ファイルへ抽出する。              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3731B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          11/11/29.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ナフコ基本情報ファイル
     SELECT   NFJOHOF   ASSIGN    TO        DA-01-VI-NFJOHOLE
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       INF-F26   INF-F01
                                            INF-F05   INF-F06
                                            INF-F07   INF-F08
                                            INF-F09
                        FILE      STATUS    INF-STATUS.
*テクノス連携ファイル　　
     SELECT   SNDTECF   ASSIGN    TO        DA-01-SNDTECNS
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    OTF-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*ナフコ基本情報ファイル
******************************************************************
 FD  NFJOHOF
                        LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOF   OF        XFDLIB
              JOINING   INF  AS   PREFIX.
*
******************************************************************
*テクノス連携ファイル
******************************************************************
 FD  SNDTECF            LABEL RECORD   IS   STANDARD
     BLOCK       CONTAINS    40        RECORDS.
     COPY     SNDTECNS  OF        XFDLIB
              JOINING   OTF       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  INF-STATUS          PIC  X(02).
     03  OTF-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
*カウンター領域
 01  WK-CNT.
     03  RD-CNT              PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT             PIC  9(08)     VALUE  ZERO.
*システム日付編集領域
 01  WK-AREA.
     03  SYS-DATE            PIC  9(06).
     03  SYS-DATEW           PIC  9(08).
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY3731B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY3731B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY3731B".
         05  FILLER          PIC  X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  AB-FILE         PIC  X(08).
         05  FILLER          PIC  X(06)  VALUE " ST = ".
         05  AB-STS          PIC  X(02).
         05  FILLER          PIC  X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  FILLER          PIC  X(07)  VALUE " SEC = ".
         05  S-NAME          PIC  X(30).
     03  MSG-IN.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " 基本情報読込件数   = ".
         05  MSG-IN01        PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " 連携データ抽出件数 = ".
         05  MSG-OUT01       PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
*ブレイクＫＥＹ領域
 01  WK-BREAK-KEY.
     03  WK-K26              PIC  X(01)   VALUE  SPACE.
*日付変換サブルーチン用領域
 01  LINK-AREA.
     03  LINK-IN-KBN         PIC   X(01).
     03  LINK-IN-YMD6        PIC   9(06).
     03  LINK-IN-YMD8        PIC   9(08).
     03  LINK-OUT-RET        PIC   X(01).
     03  LINK-OUT-YMD8       PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-KNOST              PIC   9(08).
 01  PARA-KNOED              PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-KNOST
                                       PARA-KNOED.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFJOHOF.
     MOVE      "NFJOHOLE"   TO   AB-FILE.
     MOVE      INF-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDTECF.
     MOVE      "SNDTECNS"   TO   AB-FILE.
     MOVE      OTF-STATUS   TO   AB-STS.
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
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
     STOP  RUN.
*
 GENERAL-PROCESS-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     NFJOHOF.
     OPEN     OUTPUT    SNDTECF.
     DISPLAY  MSG-START UPON CONS.
** TEST MSG
     DISPLAY  "STA-NO ="   PARA-KNOST  UPON  CONS.
     DISPLAY  "END-NO ="   PARA-KNOED  UPON  CONS.
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
*基本情報ファイルＳＴＡＲＴ
     MOVE     SPACE          TO      INF-REC.
     INITIALIZE                      INF-REC.
     MOVE     WK-K26         TO      INF-F26.
     MOVE     PARA-KNOST     TO      INF-F01.
*
     START    NFJOHOF   KEY  >=      INF-F26   INF-F01
                        INF-F05      INF-F06   INF-F07
                        INF-F08      INF-F09
         INVALID   KEY
              DISPLAY "＃管理番号無し＃＃" UPON CONS
              DISPLAY  MSG-END UPON CONS
              STOP  RUN
     END-START.
*基本情報ファイル読込
     PERFORM NFJOHOF-READ-SEC.
*終了判定
     IF   END-FLG  =  "END"
          DISPLAY "＃＃管理番号対象無し＃＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル読込　　　　　　　　　　　　　　　　　　*
****************************************************************
 NFJOHOF-READ-SEC    SECTION.
*
     READ     NFJOHOF  NEXT
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  NFJOHOF-READ-EXIT
     END-READ.
*
     IF   INF-F26  NOT  =  WK-K26
          MOVE   "END"     TO    END-FLG
          GO  TO           NFJOHOF-READ-EXIT
     END-IF.
*
     ADD      1     TO     RD-CNT.
*    管理番号の範囲チェック
     IF       INF-F01  >=    PARA-KNOST
     AND      PARA-KNOED  >= INF-F01
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  NFJOHOF-READ-EXIT
     END-IF.
*
 NFJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    テクノス連携ファイル出力
     PERFORM  SNDTECF-OUT-SEC.
 MAIN010.
*基本情報ファイル読込み
     PERFORM NFJOHOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*テクノス情報ファイル出力
****************************************************************
 SNDTECF-OUT-SEC     SECTION.
*
     MOVE     "SNDTECF-OUT-SEC"  TO  S-NAME.
*
     MOVE     SPACE          TO      OTF-REC.
     INITIALIZE                      OTF-REC.
     MOVE    INF-F01   TO   OTF-F01.
     MOVE    INF-F02   TO   OTF-F02.
     MOVE    INF-F03   TO   OTF-F03.
     MOVE    INF-F04   TO   OTF-F04.
     MOVE    INF-F05   TO   OTF-F05.
     MOVE    INF-F06   TO   OTF-F06.
     MOVE    INF-HE20  TO   OTF-F07.
     MOVE    INF-F09   TO   OTF-F08.
     MOVE    INF-F10   TO   OTF-F09.
     MOVE    0         TO   OTF-F10.
     MOVE    INF-F07   TO   OTF-F11.
     MOVE    INF-F08   TO   OTF-F12.
     MOVE    INF-F13   TO   OTF-F13.
     MOVE    INF-F14   TO   OTF-F14.
     MOVE    INF-F20   TO   OTF-F15.
     MOVE    SPACE     TO   OTF-F16.
     MOVE    "00"      TO   OTF-F17.
*
     WRITE  OTF-REC.
     ADD   1     TO    OUT-CNT.
*
 SNDTECF-OUT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE     RD-CNT     TO      MSG-IN01.
     MOVE     OUT-CNT    TO      MSG-OUT01.
     DISPLAY  MSG-IN     UPON    CONS.
     DISPLAY  MSG-OUT    UPON    CONS.
*
     CLOSE     NFJOHOF  SNDTECF.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
