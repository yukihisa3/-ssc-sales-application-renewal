# NKE0470B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NKE0470B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　検品システム　　　　　　　　　　　*
*    モジュール名　　　　：　_卸原票データ→検品用作成        *
*    作成日／更新日　　　：　2019/01/25                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　_卸原票データを読み、検品用_卸　*
*                            データを作成する。                *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NKE0470B.
*AUTHER.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K6500.
 OBJECT-COMPUTER.       FACOM-K6500.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< _卸原票Ｆ    >>*************************************
     SELECT   ZTANADT   ASSIGN  TO   DA-01-VI-ZTANADT1
                        ORGANIZATION        IS   INDEXED
                        ACCESS  MODE        IS   SEQUENTIAL
                        RECORD  KEY         IS   ZTA-F01
                        FILE    STATUS      IS   ZTA-STATUS.
****<< 検品用_卸原票Ｆ  >>**********************************
     SELECT   NTANADT   ASSIGN  TO   DA-01-VI-NTANADT1
                        ORGANIZATION        IS   INDEXED
                        ACCESS  MODE        IS   RANDOM
                        RECORD  KEY         IS   NTA-F01
                        FILE    STATUS      IS   NTA-STATUS.
****<< 商品名称マスタ >>*************************************
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE      STATUS    IS   MEI-STATUS.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<< _卸原票Ｆ    >>*************************************
 FD    ZTANADT
       LABEL  RECORD    IS      STANDARD.
       COPY   ZTANADT   OF      XFDLIB
              JOINING   ZTA     PREFIX.
****<< 検品用_卸原票Ｆ  >>**********************************
 FD    NTANADT
       LABEL  RECORD    IS      STANDARD.
       COPY   NTANADT   OF      XFDLIB
              JOINING   NTA     PREFIX.
****<< 商品名称マスタ >>*************************************
 FD    MEIMS1.
       COPY   HMEIMS    OF      XFDLIB
              JOINING   MEI     AS    PREFIX.
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  システム日付            ****
 01  SYS-DATE                PIC  9(08).
 01  SYS-TIME                PIC  9(08).
****  カウント  エリア        ****
 01  CNT-AREA                VALUE    ZERO.
     02 ZTA-CNT              PIC  9(07).
     02 SKIP-CNT             PIC  9(07).
     02 NTA-CNT              PIC  9(07).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 ZTA-STATUS           PIC  X(2).
     02 NTA-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  MEIMS1-INV-FLG          PIC  X(03)  VALUE  SPACE.
 01  ERR-CHK                 PIC  X(03)  VALUE  SPACE.
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "NKE0470B".
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
                   PROCEDURE  ZTANADT.
     MOVE   "ZTANADT "        TO    ERR-FL-ID.
     MOVE    ZTA-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NTANADT.
     MOVE   "NTANADT1 "        TO    ERR-FL-ID.
     MOVE    NTA-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  MEIMS1.
     MOVE   "MEIMS1  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
 NKE0470B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 NKE0470B-END.
     EXIT.
************************************************************
*      1.0      初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     INPUT     ZTANADT.
     OPEN     INPUT     MEIMS1.
     OPEN     I-O       NTANADT.
     INITIALIZE         CNT-AREA.
     MOVE     SPACE               TO   ERR-CHK.
*システム日付の取得
     MOVE     "3"                 TO   LINK-IN-KBN.
     ACCEPT   SYS-TIME          FROM   TIME.
     ACCEPT   LINK-IN-YMD6      FROM   DATE.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
*
     PERFORM  TANA-READ-SEC.
 INIT-END.
     EXIT.
************************************************************
*      1.1      _卸ファイルＲＥＡＤ処理                   *
************************************************************
 TANA-READ-SEC          SECTION.
*
     READ     ZTANADT
       AT  END
         MOVE     "END"      TO     END-FLG
         GO        TO        TANA-READ-END
     END-READ.
*
     ADD      1              TO     ZTA-CNT.
*取消区分
     IF  ZTA-F99  =   "9"
       GO          TO        TANA-READ-SEC
     END-IF.
*
 TANA-READ-END.
     EXIT.
************************************************************
*      2.0       メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
*商品名称マスタを索引
     PERFORM  MEIMS-RD-SEC.
*商品名称マスタ存在チェック
     IF   MEIMS1-INV-FLG = "INV"
          IF  ERR-CHK  =  SPACE
              DISPLAY NC"＜データエラーリスト＞"
                      "   "  SYS-DATE(1:4)  "/" SYS-DATE(5:2)
                      "/"    SYS-DATE(7:2)  "   "
                             SYS-TIME(1:2)  ":" SYS-TIME(3:2)
                      ":"    SYS-TIME(5:2)
              MOVE    "CHK"         TO        ERR-CHK
          END-IF
          DISPLAY NC"　【商品名称無し】：商品ＣＤ＝　"
                  ZTA-F05 " " ZTA-F06X(1:5) "-" ZTA-F06X(6:2) "-"
                  ZTA-F06X(8:1) NC"　_卸" "NO="  ZTA-F01
          ADD     1           TO                SKIP-CNT
          GO                  TO                MAIN010
     ELSE
         IF   MEI-F06  =  SPACE
              IF  ERR-CHK  =  SPACE
                  DISPLAY NC"＜データエラーリスト＞"
                          "   "  SYS-DATE(1:4)  "/" SYS-DATE(5:2)
                          "/"    SYS-DATE(7:2)  "   "
                                 SYS-TIME(1:2)  ":" SYS-TIME(3:2)
                          ":"    SYS-TIME(5:2)
                  MOVE    "CHK"         TO        ERR-CHK
              END-IF
              DISPLAY NC"　【ＪＡＮ未登録】：ＪＡＮ　＝"
                  ZTA-F05 " " ZTA-F06X(1:5) "-" ZTA-F06X(6:2) "-"
                  ZTA-F06X(8:1) NC"　_卸" "NO="  ZTA-F01
                  ADD   1     TO                SKIP-CNT
                  GO          TO                MAIN010
         END-IF
     END-IF.
*
     MOVE     SPACE           TO       NTA-REC.
     INITIALIZE                        NTA-REC.
     MOVE     ZTA-F01         TO       NTA-F01.
     MOVE     ZTA-F02         TO       NTA-F02.
     MOVE     ZTA-F03         TO       NTA-F03.
     MOVE     ZTA-F04         TO       NTA-F04.
     MOVE     ZTA-F05         TO       NTA-F05.
     MOVE     ZTA-F06         TO       NTA-F06.
     MOVE     ZTA-F07         TO       NTA-F07.
     MOVE     ZTA-F08         TO       NTA-F08.
     MOVE     ZTA-F09         TO       NTA-F09.
     MOVE     ZTA-F10         TO       NTA-F10.
     MOVE     ZTA-F11         TO       NTA-F11.
     MOVE     ZTA-F12         TO       NTA-F12.
     MOVE     ZTA-F13         TO       NTA-F13.
     MOVE     ZTA-F14         TO       NTA-F14.
     MOVE     ZTA-F15         TO       NTA-F15.
     MOVE     ZTA-F16         TO       NTA-F16.
     MOVE     ZTA-F99         TO       NTA-F17.
     MOVE     MEI-F06         TO       NTA-F88.
     WRITE    NTA-REC.
     ADD      1               TO       NTA-CNT.
*
 MAIN010.
*
     PERFORM  TANA-READ-SEC.
*
 MAIN-END.
     EXIT.
************************************************************
*      2.1.1     商品名称マスタＲＥＡＤ                    *
************************************************************
 MEIMS-RD-SEC           SECTION.
     MOVE     ZTA-F05        TO     MEI-F011.
     MOVE     ZTA-F06X       TO     MEI-F012.
     READ     MEIMS1
       INVALID       KEY
              MOVE  "INV"    TO     MEIMS1-INV-FLG
       NOT  INVALID  KEY
              MOVE  SPACE    TO     MEIMS1-INV-FLG
     END-READ.
*
 MEIMS-RD-SEC-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    ZTANADT   NTANADT   MEIMS1.
     DISPLAY "* ZTANADT    (IN)= " ZTA-CNT   " *" UPON CONS.
     DISPLAY "* ZTANADT  (SKIP)= " SKIP-CNT  " *" UPON CONS.
     DISPLAY "* NTANADT   (ADD)= " NTA-CNT   " *" UPON CONS.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
