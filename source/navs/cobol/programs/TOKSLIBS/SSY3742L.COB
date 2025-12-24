****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩシステム　　　　　　*
*    業務名　　　　　　　：　取込エラーリスト　　　            *
*    モジュール名　　　　：　取込エラーリスト発行              *
*    作成日／更新日　　　：　2011/11/30                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタより範囲を受取、取込箱数　*
*                            確定ファイルを読み、エラー分のリ　*
*                            ストを発行する。                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3742L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          11/11/30.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     YA        IS   YA
     YB        IS   YB
     YB-21     IS   YB-21
     YA-21     IS   YA-21
     STATION   IS   STAT
     CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*取込数量確定ファイル
     SELECT   TRKAKUF   ASSIGN    TO        DA-01-VI-TRKAKUL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KAK-F01   KAK-F02
                                            KAK-F03   KAK-F110
                                            KAK-F111  KAK-F112
                                            KAK-F113  KAK-F11H
                                            KAK-F115  KAK-F116
                                            KAK-F119  KAK-F11A
                                            KAK-F11B
                        FILE  STATUS   IS   KAK-STATUS.
*取込箱数確定ファイル
     SELECT   TRHAKOF   ASSIGN    TO        DA-01-VI-TRHAKOL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY                 THK-F06
                                            THK-F02   THK-F03
                                            THK-F01   THK-F121
                                            THK-F122  THK-F123
                                            THK-F124  THK-F12B
                                            THK-F126  THK-F127
                                            THK-F128
                        FILE  STATUS   IS   THK-STATUS.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
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
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC                 PIC  X(200).
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  SUTE-FLG                PIC  X(03)     VALUE  ZERO.
 01  PAGE-CNT                PIC  9(04)     VALUE  ZERO.
 01  LINE-CNT                PIC  9(02)     VALUE  ZERO.
 01  TRHAKOF-READ-CNT        PIC  9(07)     VALUE  ZERO.
 01  TRKAKUF-READ-CNT        PIC  9(07)     VALUE  ZERO.
*取込日付／時刻バックアップ
 01  WK-KEY.
     03  WK-TRDATE           PIC  9(08)     VALUE  ZERO.
     03  WK-TRTIME           PIC  9(06)     VALUE  ZERO.
*システム日付の編集
 01  WK-SYS-DATE.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*ステータス
 01  WK-ST.
     03  KAK-STATUS        PIC  X(02).
     03  THK-STATUS        PIC  X(02).
     03  PRT-STATUS        PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3742L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3742L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3742L".
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
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD00.
     03  FILLER         CHARACTER  TYPE YB-21.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(08)     VALUE  "SSY3742L".
         05  FILLER          PIC  X(30)     VALUE  SPACE.
         05  FILLER          PIC  N(16)     VALUE
         NC"＜　物流会社取込エラーリスト　＞".
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(27)     VALUE  SPACE.
         05  HD00-YYYY       PIC  9(04).
         05  FILLER          PIC  N(01)     VALUE  NC"年".
         05  HD00-MM         PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"月".
         05  HD00-DD         PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"日".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  HD00-PCNT       PIC  ZZ9.
         05  FILLER          PIC  N(01)     VALUE  NC"頁".
 01  HD01.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"取込情報".
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD01-TRDATE     PIC  9(08).
         05  FILLER          PIC  X(01)     VALUE  "-".
         05  HD01-TRTIME     PIC  9(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"部門".
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD01-TRBUMON    PIC  X(04).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"担当".
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD01-TRTANTO    PIC  X(02).
*
 01  MS01.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(09)
             VALUE NC"＜箱　数＞管理番号".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"バッチ_".
         05  FILLER          PIC  X(17)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"場所".
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"店舗".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(01)     VALUE  NC"場".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"入荷予定".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"梱包数".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(04)     VALUE  "ｱｲﾃﾑ".
         05  FILLER          PIC  N(01)     VALUE  NC"数".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"エラー情報".
 01  MS02.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(11)     VALUE  SPACE.
         05  MS02-KANRI      PIC  9(08).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-BTDATE     PIC  9(08).
         05  MS02-BTDATE-1   PIC  X(01).
         05  MS02-BTTIME     PIC  9(04).
         05  MS02-BTTIME-1   PIC  X(01).
         05  MS02-BTTORICD   PIC  9(08).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS02-BASYO      PIC  X(02).
         05  MS02-BASYO-1    PIC  X(01).
         05  MS02-BASYO-2    PIC  X(02).
         05  MS02-BASYO-3    PIC  X(01).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS02-TENPO      PIC  9(05).
         05  MS02-TENPO-1    PIC  X(01).
         05  MS02-NOUBAS     PIC  X(01).
         05  MS02-TENPO-2    PIC  X(01).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS02-NYUYOTEI   PIC  9(08).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-KONPOU     PIC  ZZZZZ9.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-AITEM      PIC  ZZZZZ9.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS02-ERRNM      PIC  N(20).
*
 01  MS03.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"＜明　細＞".
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"伝票_".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(01)     VALUE  NC"行".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"商品情報".
         05  FILLER          PIC  X(17)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"納品数".
         05  FILLER          PIC  X(01)     VALUE  "(".
         05  FILLER          PIC  N(01)     VALUE  NC"Ｂ".
         05  FILLER          PIC  X(01)     VALUE  ")".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  N(02)     VALUE  NC"ナ商".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"変Ｔ".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"取Ｍ".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"店Ｍ".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"作Ｍ".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"基１".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"基２".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"基３".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"売１".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"売２".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"連携".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"１２".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"１３".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"１４".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"箱情".
 01  MS04.
     03  FILLER.
         05  FILLER          PIC  X(11)     VALUE  SPACE.
         05  MS04-DENNO      PIC  9(09).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS04-GYO        PIC  Z9.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS04-SYOCD      PIC  X(08).
         05  MS04-SYOCD-1    PIC  X(01).
         05  MS04-JANCD      PIC  X(13).
         05  MS04-JANCD-1    PIC  X(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS04-MSNOUHIN   PIC  ZZZ,ZZ9.
         05  MS04-MSNOUHIN-1 PIC  X(01).
         05  MS04-TEISEI     PIC  X(01).
         05  MS04-TEISEI-1   PIC  X(01).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS04-ERR1       PIC  9(01).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS04-ERR2       PIC  9(01).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS04-ERR3       PIC  9(01).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS04-ERR4       PIC  9(01).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS04-ERR5       PIC  9(01).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS04-ERR6       PIC  9(01).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS04-ERR7       PIC  9(01).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS04-ERR8       PIC  9(01).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS04-ERR9       PIC  9(01).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS04-ERR10      PIC  9(01).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS04-ERR11      PIC  9(01).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS04-ERR12      PIC  9(01).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS04-ERR13      PIC  9(01).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS04-ERR14      PIC  9(01).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS04-ERR15      PIC  9(01).
*対象データなし
 01  LST-DATA-X.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃".
 01  LST-DATA-Y.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　取込データにエラーはありません。！！　＃".
 01  LST-DATA-Z.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　　　　　　　　　　　　　　　　　　　　＃".
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE1            PIC  X(136)    VALUE  ALL   "-".
 01  P-LINE2            PIC  X(136)    VALUE  ALL   "=".
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-TRDATES       PIC   9(08).
 01  PARA-TRTIMES       PIC   9(06).
 01  PARA-TRDATEE       PIC   9(08).
 01  PARA-TRTIMEE       PIC   9(06).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-TRDATES
                                       PARA-TRTIMES
                                       PARA-TRDATEE
                                       PARA-TRTIMEE.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TRKAKUF.
     MOVE      "TRKAKUL1"   TO   AB-FILE.
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
     MOVE      "TRHAKOL3"   TO   AB-FILE.
     MOVE      THK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   PRTF.
     MOVE      "PRTF    "   TO   AB-FILE.
     MOVE      PRT-STATUS   TO   AB-STS.
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
*ファイルＯＰＥＮ
     OPEN     INPUT     TRKAKUF  TRHAKOF.
     OPEN     OUTPUT    PRTF.
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
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE "END"            TO   END-FLG
          GO                    TO   INIT-EXIT
     END-IF.
*取込箱数確定ファイル読込
     PERFORM TRHAKOF-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　取込対象データ無２　＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
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
     MOVE     "1"                 TO   THK-F06.
     MOVE     PARA-TRDATES        TO   THK-F02.
     MOVE     PARA-TRTIMES        TO   THK-F03.
*
     START  TRHAKOF  KEY  IS  >=           THK-F06  THK-F02
                                  THK-F03  THK-F01  THK-F121
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
*エラー区分チェック
     IF       THK-F06  NOT =  "1"
              MOVE     "END"      TO   END-FLG
              GO                  TO   TRHAKOF-READ-EXIT
     END-IF.
*取込日付チェック
     IF       THK-F02  >=  PARA-TRDATES
     AND      THK-F02  <=  PARA-TRDATEE
              CONTINUE
     ELSE
              MOVE     "END"      TO   END-FLG
              GO                  TO   TRHAKOF-READ-EXIT
     END-IF.
*取込時刻チェック
     IF       THK-F03  >=  PARA-TRTIMES
     AND      THK-F03  <=  PARA-TRTIMEE
              CONTINUE
     ELSE
              GO                  TO   TRHAKOF-READ-SEC
     END-IF.
*
 TRHAKOF-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*取込日付／取込時刻が変わった時、改頁する。
*****DISPLAY "THK-F02   = " THK-F02     UPON CONS.
*    DISPLAY "THK-F03   = " THK-F03     UPON CONS.
*    DISPLAY "WK-TRDATE = " WK-TRDATE   UPON CONS.
*****DISPLAY "WK-TRTIME = " WK-TRTIME   UPON CONS.
     IF   THK-F02   =  WK-TRDATE
     AND  THK-F03   =  WK-TRTIME
          CONTINUE
     ELSE
          MOVE  THK-F02         TO   WK-TRDATE
          MOVE  THK-F03         TO   WK-TRTIME
**********ヘッダ行印字
          PERFORM HEAD-WT-SEC
     END-IF.
*
     MOVE    SPACE           TO     SUTE-FLG.
     PERFORM  TRKAKUF-START-SEC.
*数量確定ファイル更新
     PERFORM TRKAKUF-WT-SEC  UNTIL  SUTE-FLG = "END".
*
 MAIN-010.
     PERFORM  TRHAKOF-READ-SEC.
*
     IF   END-FLG  NOT =  "END"
     AND  THK-F02      =  WK-TRDATE
     AND  THK-F03      =  WK-TRTIME
          PERFORM  HEAD-WT1-SEC
     END-IF.
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
     START  TRKAKUF  KEY  IS  >=                    KAK-F01
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
*　　　　　　取込数量確定ファイル読込
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
     IF    THK-F128 =   KAK-F119
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-135.
     IF    KAK-F06  NOT =  "1"
           GO                   TO        TRKAKUF-READ-SEC
     END-IF.
 READ-140.
*****DISPLAY "KAK-F070 = " KAK-F070 UPON CONS.
*
 TRKAKUF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　数量訂正ファイル出力                              *
****************************************************************
 TRKAKUF-WT-SEC              SECTION.
*
     MOVE    "TRKAKUF-WT-SEC"   TO        S-NAME.
*
     PERFORM MEISAI-WT-SEC.
*
     PERFORM  TRKAKUF-READ-SEC.
*
 TRKAKUF-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ヘッダ印字
*--------------------------------------------------------------*
 HEAD-WT-SEC            SECTION.
     MOVE    "HEAD-WT-SEC"        TO   S-NAME.
*    改頁判定
     IF       PAGE-CNT  >   ZERO
              MOVE      SPACE     TO   PRT-REC
              WRITE     PRT-REC   AFTER   PAGE
     END-IF.
*    行カウンター初期化
     MOVE     ZERO                TO   LINE-CNT.
*    頁カウンター
     ADD      1                   TO   PAGE-CNT.
     MOVE     PAGE-CNT            TO   HD00-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4)      TO   HD00-YYYY.
     MOVE     SYS-DATEW(5:2)      TO   HD00-MM.
     MOVE     SYS-DATEW(7:2)      TO   HD00-DD.
*    取込日付／時刻／部門／担当者情報セット
     MOVE     THK-F02             TO   HD01-TRDATE.
     MOVE     THK-F03             TO   HD01-TRTIME.
     MOVE     THK-F05             TO   HD01-TRBUMON.
     MOVE     THK-F04             TO   HD01-TRTANTO.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  HD00  AFTER  2.
     WRITE    PRT-REC       FROM  HD01  AFTER  2.
     WRITE    PRT-REC       FROM  P-LINE1 AFTER 1.
*行カウント
     MOVE     5                   TO    LINE-CNT.
*    箱数情報をセット
     MOVE     THK-F121            TO   MS02-KANRI.
     MOVE     THK-F122            TO   MS02-BTDATE.
     MOVE     "-"                 TO   MS02-BTDATE-1.
     MOVE     THK-F123            TO   MS02-BTTIME.
     MOVE     "-"                 TO   MS02-BTTIME-1.
     MOVE     THK-F124            TO   MS02-BTTORICD.
     MOVE     THK-F125            TO   MS02-BASYO.
     MOVE     "("                 TO   MS02-BASYO-1.
     MOVE     THK-F12B            TO   MS02-BASYO-2.
     MOVE     ")"                 TO   MS02-BASYO-3.
     MOVE     THK-F126            TO   MS02-TENPO.
     MOVE     "("                 TO   MS02-TENPO-1.
     MOVE     THK-F127            TO   MS02-NOUBAS.
     MOVE     ")"                 TO   MS02-TENPO-2.
     MOVE     THK-F128            TO   MS02-NYUYOTEI.
     MOVE     THK-F129            TO   MS02-KONPOU.
     MOVE     THK-F12A            TO   MS02-AITEM.
     MOVE     NC"【取込処理でエラーがあります。確認！！】"
                                  TO   MS02-ERRNM.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
     WRITE    PRT-REC       FROM  MS02  AFTER  1.
     WRITE    PRT-REC       FROM  MS03  AFTER  2.
*行カウント
     ADD      4                   TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ヘッダ印字
*--------------------------------------------------------------*
 HEAD-WT1-SEC           SECTION.
     MOVE    "HEAD-WT1-SEC"       TO   S-NAME.
*    改頁判定
     IF       LINE-CNT  >   55
              PERFORM HEAD-WT-SEC
     END-IF.
*    箱数情報をセット
     MOVE     THK-F121            TO   MS02-KANRI.
     MOVE     THK-F122            TO   MS02-BTDATE.
     MOVE     "-"                 TO   MS02-BTDATE-1.
     MOVE     THK-F123            TO   MS02-BTTIME.
     MOVE     "-"                 TO   MS02-BTTIME-1.
     MOVE     THK-F124            TO   MS02-BTTORICD.
     MOVE     THK-F125            TO   MS02-BASYO.
     MOVE     "("                 TO   MS02-BASYO-1.
     MOVE     THK-F12B            TO   MS02-BASYO-2.
     MOVE     ")"                 TO   MS02-BASYO-3.
     MOVE     THK-F126            TO   MS02-TENPO.
     MOVE     "("                 TO   MS02-TENPO-1.
     MOVE     THK-F127            TO   MS02-NOUBAS.
     MOVE     ")"                 TO   MS02-TENPO-2.
     MOVE     THK-F128            TO   MS02-NYUYOTEI.
     MOVE     THK-F129            TO   MS02-KONPOU.
     MOVE     THK-F12A            TO   MS02-AITEM.
     MOVE     NC"【取込処理でエラーがあります。確認！！】"
                                  TO   MS02-ERRNM.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  P-LINE1 AFTER  1.
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
     WRITE    PRT-REC       FROM  MS02  AFTER  1.
     WRITE    PRT-REC       FROM  MS03  AFTER  2.
*行カウント
     ADD      5                   TO    LINE-CNT.
*
 HEAD-WT1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    箱数情報印字
*--------------------------------------------------------------*
 MEISAI-WT-SEC          SECTION.
     MOVE    "MEISAI-WT-SEC"      TO   S-NAME.
*    改頁判定
     IF       LINE-CNT  >   50
              PERFORM HEAD-WT-SEC
     END-IF.
*    数量確定情報をセット
     MOVE     KAK-F11A            TO   MS04-DENNO.
     MOVE     KAK-F11B            TO   MS04-GYO.
     MOVE     KAK-F11C            TO   MS04-SYOCD.
     MOVE     "("                 TO   MS04-SYOCD-1.
     MOVE     KAK-F11D            TO   MS04-JANCD.
     MOVE     ")"                 TO   MS04-JANCD-1.
     MOVE     KAK-F11E            TO   MS04-MSNOUHIN.
     MOVE     "("                 TO   MS04-MSNOUHIN-1.
     MOVE     KAK-F11F            TO   MS04-TEISEI.
     MOVE     ")"                 TO   MS04-TEISEI-1.
     MOVE     KAK-F070            TO   MS04-ERR1.
     MOVE     KAK-F071            TO   MS04-ERR2.
     MOVE     KAK-F072            TO   MS04-ERR3.
     MOVE     KAK-F073            TO   MS04-ERR4.
     MOVE     KAK-F074            TO   MS04-ERR5.
     MOVE     KAK-F075            TO   MS04-ERR6.
     MOVE     KAK-F076            TO   MS04-ERR7.
     MOVE     KAK-F077            TO   MS04-ERR8.
     MOVE     KAK-F078            TO   MS04-ERR9.
     MOVE     KAK-F079            TO   MS04-ERR10.
     MOVE     KAK-F07A            TO   MS04-ERR11.
     MOVE     KAK-F07B            TO   MS04-ERR12.
     MOVE     KAK-F07C            TO   MS04-ERR13.
     MOVE     KAK-F07D            TO   MS04-ERR14.
     MOVE     KAK-F07E            TO   MS04-ERR15.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  MS04  AFTER  1.
*行カウント
     ADD      1                   TO    LINE-CNT.
*
 MEISAI-WT-EXIT.
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
*
     CLOSE     TRKAKUF  TRHAKOF  PRTF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
