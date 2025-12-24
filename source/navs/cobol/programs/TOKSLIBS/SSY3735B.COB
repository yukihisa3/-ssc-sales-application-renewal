****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩシステム　　　　　　*
*    業務名　　　　　　　：　オンライン分データ処理            *
*    モジュール名　　　　：　オンライン分数量箱数データ更新　　*
*    作成日／更新日　　　：　2011/11/30                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタを受取り、チェック済の    *
*                            数量／箱数確定データをそれぞれ　　*
*                            のファイルに出力する。            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3735B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          11/11/30.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*取込数量確定ファイル
     SELECT   TRKAKUF   ASSIGN    TO        DA-01-VI-TRKAKUL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KAK-F10   KAK-F06
                                            KAK-F01   KAK-F02
                                            KAK-F03   KAK-F110
                                            KAK-F111  KAK-F112
                                            KAK-F113  KAK-F11H
                                            KAK-F115  KAK-F116
                                            KAK-F119  KAK-F11A
                                            KAK-F11B
                        FILE  STATUS   IS   KAK-STATUS.
*取込箱数確定ファイル
     SELECT   TRHAKOF   ASSIGN    TO        DA-01-VI-TRHAKOL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       THK-F11   THK-F06
                                            THK-F01   THK-F02
                                            THK-F03   THK-F121
                                            THK-F122  THK-F123
                                            THK-F124  THK-F12B
                                            THK-F126  THK-F127
                                            THK-F128
                        FILE  STATUS   IS   THK-STATUS.
*商品変換テーブル
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01
                                            TBL-F02
                        FILE  STATUS   IS   TBL-STATUS.
*---<<  商品名称マスタ  >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-STATUS.
*基本情報ファイル
     SELECT   NFJOHOF   ASSIGN    TO        DA-01-VI-NFJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JOH-F01  JOH-F05
                                            JOH-F06  JOH-F07
                                            JOH-F08  JOH-F09
                        FILE STATUS    IS   JOH-STATUS.
*売上伝票ファイル
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F02   DEN-F04
                                            DEN-F051
***サーバー統合後のキーを追加　１２月開始時はコメントアウト
                                            DEN-F07
                                            DEN-F112
*************************************************************
                                            DEN-F03
                        FILE STATUS    IS   DEN-STATUS.
*在庫マスタ
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       ZAI-F01
                                            ZAI-F02
                                            ZAI-F03
                        FILE STATUS    IS   ZAI-STATUS.
*数量訂正ファイル
     SELECT   NFSUTEF   ASSIGN    TO        DA-01-VI-NFSUTEL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       STE-F01   STE-F05
                                            STE-F08   STE-F06
                                            STE-F07   STE-F09
                        FILE  STATUS   IS   STE-STATUS.
*箱数ファイル
     SELECT   NFHAKOF   ASSIGN    TO        DA-01-VI-NFHAKOL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HAK-F01   HAK-F05
                                            HAK-F08   HAK-F06
                                            HAK-F07
                        FILE STATUS    IS   HAK-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    取込数量確定ファイル
******************************************************************
 FD  TRKAKUF            LABEL RECORD   IS   STANDARD.
     COPY     TRKAKUF   OF        XFDLIB
              JOINING   KAK       PREFIX.
******************************************************************
*    取込箱数確定ファイル
******************************************************************
 FD  TRHAKOF            LABEL RECORD   IS   STANDARD.
     COPY     TRHAKOF   OF        XFDLIB
              JOINING   THK       PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  HMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
*    ナフコ基本情報ファイル
******************************************************************
 FD  NFJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOF   OF        XFDLIB
              JOINING   JOH       PREFIX.
******************************************************************
*    売上伝票ファイル
******************************************************************
 FD  SHTDENF            LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
******************************************************************
*    在庫マスタ
******************************************************************
 FD  ZAMZAIF            LABEL RECORD   IS   STANDARD.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
******************************************************************
*    数量訂正ファイル　　　
******************************************************************
 FD  NFSUTEF            LABEL RECORD   IS   STANDARD.
     COPY     NFSUTEF   OF        XFDLIB
              JOINING   STE       PREFIX.
******************************************************************
*    出荷情報エラーファイル
******************************************************************
 FD  NFHAKOF
                        LABEL RECORD   IS   STANDARD.
     COPY     NFHAKOF   OF        XFDLIB
              JOINING   HAK  AS   PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*基本情報Ｆワーク
     COPY  NFJOHOF  OF  XFDLIB  JOINING  WK   AS  PREFIX.
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  SUTE-FLG                PIC  X(03)     VALUE  ZERO.
 01  DEL-FLG                 PIC  X(03)     VALUE  ZERO.
 01  KEP-FLG                 PIC  X(01)     VALUE  ZERO.
 01  SKIP-CNT                PIC  9(07)     VALUE  ZERO.
 01  TRHAKOF-READ-CNT        PIC  9(07)     VALUE  ZERO.
 01  TRKAKUF-READ-CNT        PIC  9(07)     VALUE  ZERO.
 01  HAK-ADD-CNT             PIC  9(07)     VALUE  ZERO.
 01  HAK-UPD-CNT             PIC  9(07)     VALUE  ZERO.
 01  STE-DATA-CNT            PIC  9(07)     VALUE  ZERO.
 01  STE-GYO-CNT             PIC  9(07)     VALUE  ZERO.
 01  STE-WT-CNT              PIC  9(07)     VALUE  ZERO.
 01  STE-REWT-CNT            PIC  9(07)     VALUE  ZERO.
 01  KIHON-ADD-CNT           PIC  9(07)     VALUE  ZERO.
 01  KIHON-UPD-CNT           PIC  9(07)     VALUE  ZERO.
 01  DEN-UPD-CNT             PIC  9(07)     VALUE  ZERO.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  ZAMZAIF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  NFHAKOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  NFSUTEF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  NFJOHOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  SHTDENF-INV-FLG         PIC  X(03)     VALUE  SPACE.
*
 01  WRK-AREA.
     03  WRK-TEISUU          PIC S9(09)V99  VALUE  ZERO.
     03  WRK-MAETEISUU       PIC S9(09)V99  VALUE  ZERO.
*計算領域
 01  WRK-AREA2.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  KAK-STATUS        PIC  X(02).
     03  THK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  ZAI-STATUS        PIC  X(02).
     03  STE-STATUS        PIC  X(02).
     03  HAK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3735B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3735B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3735B".
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
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-TRDATE        PIC   9(08).
 01  PARA-TRTIME        PIC   9(06).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-TRDATE
                                       PARA-TRTIME.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TRKAKUF.
     MOVE      "TRKAKUL2"   TO   AB-FILE.
     MOVE      KAK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TRHAKOF.
     MOVE      "TRHAKOL2"   TO   AB-FILE.
     MOVE      THK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HSHOTBL.
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
                        PROCEDURE   HMEIMS.
     MOVE      "MEIMS1  "   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFJOHOF.
     MOVE      "NFJOHOL1"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENF.
     MOVE      "SHTDENL1"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZAMZAIF.
     MOVE      "ZAMZAIL1"   TO   AB-FILE.
     MOVE      ZAI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSUTEF.
     MOVE      "NFSUTEL4"   TO   AB-FILE.
     MOVE      STE-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFHAKOF.
     MOVE      "NFHAKOL4"   TO   AB-FILE.
     MOVE      HAK-STATUS   TO   AB-STS.
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
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     INITIALIZE                   WRK-AREA.
     OPEN     I-O       TRKAKUF  TRHAKOF  NFJOHOF  SHTDENF
                        ZAMZAIF  NFSUTEF  NFHAKOF.
     OPEN     INPUT     HSHOTBL  HMEIMS.
*
     DISPLAY  MSG-START UPON CONS.
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
*取込箱数確定ファイルスタート
     PERFORM  TRHAKOF-START-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　取込対象データ無１　＃＃"  UPON CONS
          MOVE "END"            TO   END-FLG
          GO                    TO   INIT-EXIT
     END-IF.
*取込箱数確定ファイル読込
     PERFORM TRHAKOF-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　取込対象データ無２　＃＃"  UPON CONS
          MOVE "END"            TO   END-FLG
          GO                    TO   INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    取込箱数確定ファイルスタート
****************************************************************
 TRHAKOF-START-SEC          SECTION.
*
     MOVE    "TRHAKOF-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   THK-REC.
     INITIALIZE                        THK-REC.
*
     MOVE     SPACE               TO   THK-F11.
     MOVE     SPACE               TO   THK-F06.
     MOVE     "1"                 TO   THK-F01.
     MOVE     PARA-TRDATE         TO   THK-F02.
     MOVE     PARA-TRTIME         TO   THK-F03.
*
     START  TRHAKOF  KEY  IS  >=  THK-F11  THK-F06  THK-F01
                                  THK-F02  THK-F03  THK-F121
                                  THK-F122 THK-F123 THK-F124
                                  THK-F12B THK-F126 THK-F127
                                  THK-F128
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 TRHAKOF-START-EXIT.
     EXIT.
*
****************************************************************
*    取込箱数確定ファイル読込
****************************************************************
 TRHAKOF-READ-SEC           SECTION.
*
     MOVE    "TRHAKOF-READ-SEC"   TO   S-NAME.
*
     READ     TRHAKOF  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   TRHAKOF-READ-EXIT
     END-READ.
*件数カウント
     ADD      1                   TO   TRHAKOF-READ-CNT.
*更新区分チェック
     IF       THK-F11  =  "1"
              MOVE     "END"      TO   END-FLG
              GO                  TO   TRHAKOF-READ-EXIT
     END-IF.
*エラー区分チェック
     IF       THK-F06  =  "1"
              MOVE     "END"      TO   END-FLG
              GO                  TO   TRHAKOF-READ-EXIT
     END-IF.
*オンライン／手書区分チェック
     IF       THK-F01  NOT =  "1"
              MOVE     "END"      TO   END-FLG
              GO                  TO   TRHAKOF-READ-EXIT
     END-IF.
*取込日付チェック
     IF       THK-F02  NOT =  PARA-TRDATE
     OR       THK-F03  NOT =  PARA-TRTIME
              MOVE     "END"      TO   END-FLG
              GO                  TO   TRHAKOF-READ-EXIT
     END-IF.
*
 TRHAKOF-READ-EXIT.
     EXIT.
*
****************************************************************
*    箱数確定ファイル読込
****************************************************************
 NFHAKOF-READ-SEC           SECTION.
*
     MOVE    "NFHAKOF-READ-SEC"   TO   S-NAME.
*
     MOVE     THK-F121            TO   HAK-F01.     *>管理番号
     MOVE     THK-F12B            TO   HAK-F05.     *>倉庫ＣＤ
     MOVE     THK-F128            TO   HAK-F08.     *>店着日
     MOVE     THK-F126            TO   HAK-F06.     *>店舗ＣＤ
     MOVE     THK-F127            TO   HAK-F07.     *>納品場所
     READ     NFHAKOF
              INVALID     MOVE  "INV"  TO   NFHAKOF-INV-FLG
              NOT INVALID MOVE  SPACE  TO   NFHAKOF-INV-FLG
     END-READ.
*
 NFHAKOF-READ-EXIT.
     EXIT.
****************************************************************
*    数量確定ファイル読込
****************************************************************
 NFSUTEF-READ-SEC           SECTION.
*
     MOVE    "NFSUTEF-READ-SEC"   TO   S-NAME.
*
     MOVE     KAK-F110            TO   STE-F01.     *>管理番号
     MOVE     KAK-F11H            TO   STE-F05.     *>倉庫ＣＤ
     MOVE     KAK-F119            TO   STE-F08.     *>店着日
     MOVE     KAK-F115            TO   STE-F06.     *>店舗ＣＤ
     MOVE     KAK-F116            TO   STE-F07.     *>納品場所
     MOVE     KAK-F11A            TO   STE-F09.     *>伝票番号
*****DISPLAY "110 = " KAK-F110 " - " HAK-F01.
*    DISPLAY "11H = " KAK-F11H " - " HAK-F05.
*    DISPLAY "119 = " KAK-F119 " - " HAK-F08.
*    DISPLAY "115 = " KAK-F115 " - " HAK-F06.
*    DISPLAY "116 = " KAK-F116 " - " HAK-F07.
*****DISPLAY "11A = " KAK-F11A " - " HAK-F09.
     READ     NFSUTEF
              INVALID     MOVE  "INV"  TO   NFSUTEF-INV-FLG
              NOT INVALID MOVE  SPACE  TO   NFSUTEF-INV-FLG
     END-READ.
*
 NFSUTEF-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*箱数確定ファイルの存在チェック
     PERFORM  NFHAKOF-READ-SEC.
*****ＴＲＡＮＴＲＡＮ連携済は更新対象としない。
     IF   NFHAKOF-INV-FLG = SPACE
          IF  HAK-F98 = "1"
              GO                 TO   MAIN-010
          END-IF
     END-IF.
*取込数量確定ファイルスタート
     MOVE    SPACE               TO   SUTE-FLG.
     PERFORM  TRKAKUF-START-SEC.
*数量確定ファイル更新
     MOVE    ZERO                TO   STE-GYO-CNT.
     PERFORM NFSUTEF-WT-SEC  UNTIL  SUTE-FLG = "END".
*箱数確定ファイル出力（明細が出力ない場合は箱数Ｆは作成しない）
*****DISPLAY "CHK = " STE-GYO-CNT "-" THK-F126.
     IF  STE-GYO-CNT > ZERO
         IF  NFHAKOF-INV-FLG = "INV"
             MOVE  SPACE        TO  HAK-REC
             INITIALIZE             HAK-REC
             MOVE  THK-F121     TO  HAK-F01
             MOVE  THK-F122     TO  HAK-F02
             MOVE  THK-F123     TO  HAK-F03
             MOVE  THK-F124     TO  HAK-F04
             MOVE  THK-F12B     TO  HAK-F05
             MOVE  THK-F126     TO  HAK-F06
             MOVE  THK-F127     TO  HAK-F07
             MOVE  THK-F128     TO  HAK-F08
             MOVE  THK-F129     TO  HAK-F09
             MOVE  THK-F12A     TO  HAK-F10
             MOVE  SPACE        TO  HAK-F98
             MOVE  ZERO         TO  HAK-F99
             WRITE HAK-REC
             ADD   1            TO  HAK-ADD-CNT
         ELSE
             MOVE  THK-F121     TO  HAK-F01
             MOVE  THK-F122     TO  HAK-F02
             MOVE  THK-F123     TO  HAK-F03
             MOVE  THK-F124     TO  HAK-F04
             MOVE  THK-F12B     TO  HAK-F05
             MOVE  THK-F126     TO  HAK-F06
             MOVE  THK-F127     TO  HAK-F07
             MOVE  THK-F128     TO  HAK-F08
             MOVE  THK-F129     TO  HAK-F09
             MOVE  THK-F12A     TO  HAK-F10
             MOVE  SPACE        TO  HAK-F98
             MOVE  ZERO         TO  HAK-F99
             REWRITE HAK-REC
             ADD   1            TO  HAK-UPD-CNT
         END-IF
*********更新区分セット
         MOVE        "1"        TO  THK-F11
         REWRITE  THK-REC
     ELSE
         MOVE        "1"        TO  THK-F06
         REWRITE  THK-REC
     END-IF.
*
 MAIN-010.
     PERFORM  TRHAKOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*　　取込数量確定ファイル　スタート
****************************************************************
 TRKAKUF-START-SEC     SECTION.
*
     MOVE    "TRKAKUF-READ-SEC"  TO   S-NAME.
*
     MOVE     SPACE              TO   KAK-REC.
     INITIALIZE                       KAK-REC.
*
     MOVE     THK-F11            TO   KAK-F10.
     MOVE     THK-F06            TO   KAK-F06.
     MOVE     THK-F01            TO   KAK-F01.
     MOVE     THK-F02            TO   KAK-F02.
     MOVE     THK-F03            TO   KAK-F03.
     MOVE     THK-F121           TO   KAK-F110.
     MOVE     THK-F122           TO   KAK-F111.
     MOVE     THK-F123           TO   KAK-F112.
     MOVE     THK-F124           TO   KAK-F113.
     MOVE     THK-F12B           TO   KAK-F11H.
     MOVE     THK-F126           TO   KAK-F115.
     MOVE     THK-F127           TO   KAK-F116.
     MOVE     THK-F128           TO   KAK-F119.
*
     START  TRKAKUF  KEY  IS  >=  KAK-F10  KAK-F06  KAK-F01
                                  KAK-F02  KAK-F03  KAK-F110
                                  KAK-F111 KAK-F112 KAK-F113
                                  KAK-F11H KAK-F115 KAK-F116
                                  KAK-F119 KAK-F11A KAK-F11B
           INVALID
           MOVE  "END"           TO      SUTE-FLG
           GO                    TO      TRKAKUF-START-EXIT
     END-START.
*取込数量確定ファイル読込
     PERFORM  TRKAKUF-READ-SEC.
*
 TRKAKUF-START-EXIT.
     EXIT.
****************************************************************
*　　　　　　箱数ファイル出力                                  *
****************************************************************
 TRKAKUF-READ-SEC            SECTION.
*
     MOVE    "TRKAKUF-READ-SEC" TO        S-NAME.
*
     READ  TRKAKUF  AT  END
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-READ.
*件数カウント
     ADD      1                   TO   TRKAKUF-READ-CNT.
*対象データチェック
*****DISPLAY  "11  = " THK-F11  " = " KAK-F10    UPON CONS.
*    DISPLAY  "06  = " THK-F06  " = " KAK-F06    UPON CONS.
*    DISPLAY  "01  = " THK-F01  " = " KAK-F01    UPON CONS.
*    DISPLAY  "02  = " THK-F02  " = " KAK-F02    UPON CONS.
*    DISPLAY  "03  = " THK-F03  " = " KAK-F03    UPON CONS.
*    DISPLAY  "121 = " THK-F121 " = " KAK-F110   UPON CONS.
*    DISPLAY  "122 = " THK-F122 " = " KAK-F111   UPON CONS.
*    DISPLAY  "123 = " THK-F123 " = " KAK-F112   UPON CONS.
*    DISPLAY  "124 = " THK-F124 " = " KAK-F113   UPON CONS.
*    DISPLAY  "12B = " THK-F12B " = " KAK-F11H   UPON CONS.
*    DISPLAY  "126 = " THK-F126 " = " KAK-F115   UPON CONS.
*    DISPLAY  "127 = " THK-F127 " = " KAK-F116   UPON CONS.
*****DISPLAY  "128 = " THK-F128 " = " KAK-F119   UPON CONS.
 READ-010.
     IF    THK-F11  =   KAK-F10
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-020.
     IF    THK-F06  =   KAK-F06
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-030.
     IF    THK-F01  =   KAK-F01
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-040.
     IF    THK-F02  =   KAK-F02
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-050.
     IF    THK-F03  =   KAK-F03
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-060.
     IF    THK-F121 =   KAK-F110
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-070.
     IF    THK-F122 =   KAK-F111
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-080.
     IF    THK-F123 =   KAK-F112
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-090.
     IF    THK-F124 =   KAK-F113
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-100.
     IF    THK-F12B =   KAK-F11H
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-110.
     IF    THK-F126 =   KAK-F115
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-120.
     IF    THK-F127 =   KAK-F116
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-130.
*****DISPLAY  "128 = " THK-F128 " = " KAK-F119   UPON CONS.
     IF    THK-F128 =   KAK-F119
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-140.
*
 TRKAKUF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　数量訂正ファイル出力                              *
****************************************************************
 NFSUTEF-WT-SEC              SECTION.
*
     MOVE    "NFSUTEF-WT-SEC"   TO        S-NAME.
 WT-010.
*数量確定ファイル存在チェック
     PERFORM  NFSUTEF-READ-SEC.
*****既にＴＲＡＮＴＲＡＮ連携済の場合は対象としない。
 WT-020.
     IF  NFSUTEF-INV-FLG = SPACE
         IF  STE-F98 = "1"
             GO             TO        NFSUTEF-WT-010
         END-IF
     END-IF.
 WT-030.
*ナフコ基本情報ファイルを更新する。
     PERFORM NFJOHOF-READ-SEC.
 WT-040.
*売上伝票ファイルを更新する。
     PERFORM SHTDENF-READ-SEC.
 WT-050.
*****ファイル更新を行なう。
*****DISPLAY "NFSUTEF-INV-FLG = " NFSUTEF-INV-FLG.
     IF  NFSUTEF-INV-FLG = "INV"
*********DISPLAY "ABC"
         MOVE  SPACE        TO        STE-REC
         INITIALIZE                   STE-REC
         MOVE  KAK-F110     TO        STE-F01
         MOVE  KAK-F111     TO        STE-F02
         MOVE  KAK-F112     TO        STE-F03
         MOVE  KAK-F113     TO        STE-F04
         MOVE  KAK-F11H     TO        STE-F05
         MOVE  KAK-F115     TO        STE-F06
         MOVE  KAK-F116     TO        STE-F07
         MOVE  KAK-F119     TO        STE-F08
         MOVE  KAK-F11A     TO        STE-F09
         MOVE  KAK-F11D     TO        STE-F10
         MOVE  KAK-F11E     TO        STE-F11
         EVALUATE  KAK-F11F
             WHEN  "1"  MOVE "01" TO  STE-F12
             WHEN  "2"  MOVE "02" TO  STE-F12
             WHEN  "3"  MOVE "03" TO  STE-F12
             WHEN  "4"  MOVE "04" TO  STE-F12
             WHEN  "5"  MOVE "05" TO  STE-F12
             WHEN  "6"  MOVE "06" TO  STE-F12
             WHEN  "7"  MOVE "07" TO  STE-F12
             WHEN  "8"  MOVE "09" TO  STE-F12
         END-EVALUATE
         MOVE  KAK-F119     TO        STE-F13
         MOVE  KAK-F118     TO        STE-F14
         MOVE  "1"          TO        STE-F15
*********MOVE  KAK-F11G     TO        STE-FIL(1:2)
         MOVE  KAK-F11C     TO        STE-F96
         MOVE  KAK-F117     TO        STE-F97
         MOVE  SPACE        TO        STE-F98
         MOVE  ZERO         TO        STE-F99
         WRITE STE-REC
         ADD   1            TO        STE-DATA-CNT
         ADD   1            TO        STE-GYO-CNT
         ADD   1            TO        STE-WT-CNT
     ELSE
*********DISPLAY "CBA"
         MOVE  KAK-F110     TO        STE-F01
         MOVE  KAK-F111     TO        STE-F02
         MOVE  KAK-F112     TO        STE-F03
         MOVE  KAK-F113     TO        STE-F04
         MOVE  KAK-F11H     TO        STE-F05
         MOVE  KAK-F115     TO        STE-F06
         MOVE  KAK-F116     TO        STE-F07
         MOVE  KAK-F119     TO        STE-F08
         MOVE  KAK-F11A     TO        STE-F09
         MOVE  KAK-F11B     TO        STE-F10
         MOVE  KAK-F11E     TO        STE-F11
         EVALUATE  KAK-F11F
             WHEN  "1"  MOVE "01" TO  STE-F12
             WHEN  "2"  MOVE "02" TO  STE-F12
             WHEN  "3"  MOVE "03" TO  STE-F12
             WHEN  "4"  MOVE "04" TO  STE-F12
             WHEN  "5"  MOVE "05" TO  STE-F12
             WHEN  "6"  MOVE "06" TO  STE-F12
             WHEN  "7"  MOVE "07" TO  STE-F12
             WHEN  "8"  MOVE "09" TO  STE-F12
         END-EVALUATE
         MOVE  KAK-F119     TO        STE-F13
         MOVE  KAK-F118     TO        STE-F14
         MOVE  "1"          TO        STE-F15
*********MOVE  KAK-F11G     TO        STE-FIL(1:2)
         MOVE  KAK-F11C     TO        STE-F96
         MOVE  KAK-F117     TO        STE-F97
         MOVE  SPACE        TO        STE-F98
         MOVE  ZERO         TO        STE-F99
         REWRITE STE-REC
         ADD   1            TO        STE-DATA-CNT
         ADD   1            TO        STE-GYO-CNT
         ADD   1            TO        STE-REWT-CNT
     END-IF.
  WT-060.
*更新区分セット
     MOVE        "1"        TO        KAK-F10.
     REWRITE  KAK-REC.
*
 NFSUTEF-WT-010.
     PERFORM  TRKAKUF-READ-SEC.
*
 NFSUTEF-WT-EXIT.
     EXIT.
****************************************************************
*　　ナフコ基本情報ファイル更新                                *
****************************************************************
 NFJOHOF-READ-SEC            SECTION.
*
     MOVE    "NFJOHOF-READ-SEC" TO        S-NAME.
*
     MOVE  KAK-F110             TO        JOH-F01.
     MOVE  KAK-F114             TO        JOH-F05.
     MOVE  KAK-F115             TO        JOH-F06.
     MOVE  KAK-F11A             TO        JOH-F07.
     MOVE  KAK-F11B             TO        JOH-F08.
     MOVE  KAK-F117             TO        JOH-F09.
*
     READ  NFJOHOF
           INVALID     MOVE "INV"  TO     NFJOHOF-INV-FLG
           NOT INVALID MOVE SPACE  TO     NFJOHOF-INV-FLG
     END-READ.
*****DISPLAY "NFJOHOF-INV-FLG = " NFJOHOF-INV-FLG UPON CONS.
*
     IF  NFJOHOF-INV-FLG = "INV"
         GO                     TO        NFJOHOF-READ-EXIT
     END-IF.
*出荷場所が異なった場合
*****DISPLAY "KAK-F11H = " KAK-F11H UPON CONS.
*****DISPLAY "JOH-F05  = " JOH-F05  UPON CONS.
     IF  KAK-F11H  NOT =  JOH-F05
*********基本情報ＦをＢＫし、基本情報Ｆを削除する。
         MOVE  JOH-REC          TO        WK-REC
         DELETE NFJOHOF
         MOVE  "DEL"            TO        DEL-FLG
         MOVE  SPACE            TO        JOH-REC
         INITIALIZE                       JOH-REC
         MOVE  WK-REC           TO        JOH-REC
         MOVE  KAK-F11H         TO        JOH-F05
*********出荷日と入荷予定日を更新する。
         MOVE  KAK-F118         TO        JOH-F10
         MOVE  KAK-F119         TO        JOH-F11
     ELSE
*********出荷日と入荷予定日を更新する。
         MOVE  KAK-F118         TO        JOH-F10
         MOVE  KAK-F119         TO        JOH-F11
         MOVE  SPACE            TO        DEL-FLG
     END-IF.
*納品数量を更新する。
     IF  KAK-F11E  NOT =  JOH-F20
         MOVE  KAK-F11E         TO        JOH-F20
     END-IF.
*基本情報ファイルを更新する。
     IF  DEL-FLG  =  "DEL"
         WRITE  JOH-REC
         ADD    1               TO        KIHON-ADD-CNT
     ELSE
         REWRITE JOH-REC
         ADD    1               TO        KIHON-UPD-CNT
     END-IF.
*
 NFJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　売上伝票ファイル更新　　　                                *
****************************************************************
 SHTDENF-READ-SEC            SECTION.
*
     MOVE    "SHTDENF-READ-SEC" TO        S-NAME.
*
     MOVE  KAK-F111             TO        DEN-F46.
     MOVE  KAK-F112             TO        DEN-F47.
     MOVE  KAK-F113             TO        DEN-F01.
     MOVE  KAK-F114             TO        DEN-F48.
     MOVE  KAK-F11A             TO        DEN-F02.
     MOVE  ZERO                 TO        DEN-F04.
     MOVE  40                   TO        DEN-F051.
*****統合後、索引方法 START （開始はコメントアウト）
     MOVE  KAK-F115             TO        DEN-F07.
     MOVE  KAK-F117             TO        DEN-F112.
*****統合後、索引方法 END
     MOVE  KAK-F11B             TO        DEN-F03.
*
     READ  SHTDENF
           INVALID     MOVE "INV"  TO     SHTDENF-INV-FLG
           NOT INVALID MOVE SPACE  TO     SHTDENF-INV-FLG
     END-READ.
*
     IF  SHTDENF-INV-FLG = "INV"
         GO                     TO        SHTDENF-READ-EXIT
     END-IF.
*数量が異なった場合
*****在庫更新
     IF  KAK-F11E  NOT =  DEN-F15
         MOVE     DEN-F15       TO        WRK-MAETEISUU
         MOVE     KAK-F11E      TO        WRK-TEISUU
         PERFORM  ZAIKO-SEC
         MOVE     KAK-F11E      TO        DEN-F15
         COMPUTE  DEN-F181  =  DEN-F15  *  DEN-F172
         COMPUTE  DEN-F182  =  DEN-F15  *  DEN-F173
     END-IF.
*****売上伝票Ｆの数量を更新／付番ＦＬＧ更新
     MOVE         "9"           TO        DEN-F276.
     REWRITE  DEN-REC.
     ADD           1            TO        DEN-UPD-CNT.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*              在庫引当                                        *
****************************************************************
 ZAIKO-SEC              SECTION.
*
     MOVE   "ZAIKO-SEC"      TO   S-NAME.
     MOVE    SPACE           TO   KEP-FLG.
*商品在庫マスタ存在チェック
     MOVE    DEN-F08         TO   ZAI-F01.
     MOVE    DEN-F1411       TO   ZAI-F021.
     MOVE    DEN-F1412       TO   ZAI-F022.
     MOVE    DEN-F49         TO   ZAI-F03.
     READ    ZAMZAIF
             INVALID
             PERFORM   ZAIKO-UPDATE1-SEC
             NOT  INVALID
             PERFORM   ZAIKO-UPDATE2-SEC
     END-READ.
*
 ZAIKO-EXIT.
     EXIT.
****************************************************************
*              在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE1-SEC      SECTION.
*
     MOVE    "ZAIKO-UPDATE1-SEC" TO   S-NAME.
*商品在庫Ｍが未存在の為、在庫マスタ作成
     MOVE    "1"            TO   KEP-FLG.
*商品在庫マスタ初期化
     MOVE     SPACE         TO   ZAI-REC.
     INITIALIZE                  ZAI-REC.
*商品在庫マスタ項目セット
     MOVE     DEN-F08       TO   ZAI-F01.
     MOVE     DEN-F1411     TO   ZAI-F021.
     MOVE     DEN-F1412     TO   ZAI-F022.
     MOVE     DEN-F49       TO   ZAI-F03.
*未出庫数＝未出庫数＋数量
     COMPUTE  ZAI-F27       =    ZAI-F27  +  WRK-TEISUU.
*商品名称マスタ読込み
      PERFORM   HMEIMS-READ-SEC.
*商品名称マスタ存在チェック
      IF  HMEIMS-INV-FLG  =  SPACE
          MOVE  KAK-F113      TO   ZAI-F29
          MOVE  MEI-F031      TO   ZAI-F30
          MOVE  SYS-DATEW     TO   ZAI-F98
          MOVE  SYS-DATEW     TO   ZAI-F99
          WRITE ZAI-REC
********  DISPLAY "SYS-DATEW 1 = " SYS-DATEW UPON CONS
      END-IF.
*
 ZAIKO-UPDATE1-EXIT.
      EXIT.
****************************************************************
*              在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE2-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE2-SEC" TO   S-NAME.
     INITIALIZE    WRK-AREA2.
*
     IF  DEN-F27D      =     1
*        引当済数に数量減算
         COMPUTE  ZAI-F28   =   ZAI-F28  -  WRK-MAETEISUU
*        未出庫数に数量減算
         COMPUTE  ZAI-F27   =   ZAI-F27  -  WRK-MAETEISUU
     ELSE
*        未出庫数に数量減算
         COMPUTE  ZAI-F27   =   ZAI-F27  -  WRK-MAETEISUU
     END-IF.
*
*引当後在庫数チェック
*    現在庫数－引当済数＝引当可能在庫数
     COMPUTE   WRK-ZAI   =   ZAI-F04  -  ZAI-F28.
*    引当可能在庫数－発注数量＝引当後在庫数
     COMPUTE   WRK-HIK   =   WRK-ZAI  -  WRK-TEISUU.
     IF  WRK-HIK  <  0
         MOVE      "1"      TO   KEP-FLG
*        未出庫数に数量加算
         COMPUTE  ZAI-F27   =   ZAI-F27  +  WRK-TEISUU
         MOVE     SYS-DATEW      TO    ZAI-F99
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
*******  DISPLAY "SYS-DATEW 2 = " SYS-DATEW UPON CONS
     ELSE
*        引当済数に数量加算
         COMPUTE  ZAI-F28   =   ZAI-F28  +  WRK-TEISUU
*        未出庫数に数量加算
         COMPUTE  ZAI-F27   =   ZAI-F27  +  WRK-TEISUU
         MOVE     SYS-DATEW      TO    ZAI-F99
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
*******  DISPLAY "SYS-DATEW 3 = " SYS-DATEW UPON CONS
     END-IF.
*
 ZAIKO-UPDATE2-EXIT.
     EXIT.
****************************************************************
*                商品変換テーブル読込み                        *
****************************************************************
 HSHOTBL-READ-SEC          SECTION.
*
     MOVE      "HSHOTBL-READ-SEC" TO    S-NAME.
*
     MOVE      KAK-F113    TO     TBL-F01.
     MOVE      KAK-F11C    TO     TBL-F02.
     READ      HSHOTBL
               INVALID
               MOVE      "INV"    TO    HSHOTBL-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*                商品名称マスタ読込み                          *
****************************************************************
 HMEIMS-READ-SEC           SECTION.
*
     MOVE      "HMEIMS-READ-SEC"  TO    S-NAME.
     MOVE      SPACE       TO     HMEIMS-INV-FLG.
*
     MOVE      TBL-F031    TO     MEI-F011.
     MOVE      TBL-F032    TO     MEI-F012.
     READ      HMEIMS
               INVALID
               MOVE      "INV"    TO    HMEIMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数印字
*取込箱数確定ファイル読込
     DISPLAY "TRHAKOF   READ CNT = " TRHAKOF-READ-CNT UPON CONS.
*取込数量確定ファイル読込
     DISPLAY "TRKAKUF   READ CNT = " TRKAKUF-READ-CNT UPON CONS.
*ＳＫＩＰ
*****DISPLAY "SKIP CNT           = " SKIP-CNT         UPON CONS.
*数量確定ファイル追加
     DISPLAY "NFSUTEF   WRT  CNT = " STE-WT-CNT       UPON CONS.
*数量確定ファイル更新
     DISPLAY "NFSUTEF   UPD  CNT = " STE-REWT-CNT     UPON CONS.
*箱数確定ファイル追加
     DISPLAY "NFHAKOF   WRT  CNT = " HAK-ADD-CNT      UPON CONS.
*箱数確定ファイル更新
     DISPLAY "NFHAKOF   UPD  CNT = " HAK-UPD-CNT      UPON CONS.
*基本情報ファイル追加
     DISPLAY "NFJOHOF   WRT  CNT = " KIHON-ADD-CNT    UPON CONS.
*基本情報ファイル更新
     DISPLAY "NFJOHOF   UPD  CNT = " KIHON-UPD-CNT    UPON CONS.
*売上伝票ファイル更新
     DISPLAY "SHTDENF   UPD  CNT = " DEN-UPD-CNT      UPON CONS.
*
     CLOSE     TRKAKUF  TRHAKOF  HSHOTBL  HMEIMS  NFJOHOF
               SHTDENF  ZAMZAIF  NFSUTEF  NFHAKOF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
